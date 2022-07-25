use ::alloc::alloc;
use core::alloc::Layout;
use core::cell::Cell;
use core::cmp;
use core::mem;
use core::panic::RefUnwindSafe;
use core::ptr;
use core::slice;
use std::process;

struct Header {
    cap: usize,
    len: usize,
    refcount: Cell<usize>,
}

pub(crate) struct RcVec<T> {
    first: *mut T,
}

pub(crate) struct RcVecBuilder<T> {
    first: *mut T,
}

pub(crate) struct RcVecMut<'a, T> {
    first: &'a mut *mut T,
}

#[derive(Clone)]
pub(crate) struct RcVecIntoIter<T> {
    first: *mut T,
    cur: *mut T,
}

unsafe fn header<'a, T>(first: *mut T) -> &'a Header {
    unsafe { &*(first as *const Header).offset(-1) }
}

unsafe fn header_mut<'a, T>(first: *mut T) -> &'a mut Header {
    unsafe { &mut *(first as *mut Header).offset(-1) }
}

unsafe fn allocation_layout<T>(first: *mut T) -> (*mut u8, Layout) {
    // Alignment of the allocation.
    let align = cmp::max(mem::align_of::<T>(), mem::align_of::<Header>());

    // Round up Header's size to a multiple of allocation's alignment.
    let mut header_size = mem::size_of::<Header>();
    header_size = (header_size + align - 1) / align * align;

    let allocation = unsafe { (first as *mut u8).sub(header_size) };
    let total_size = header_size + unsafe { header(first).cap } * mem::size_of::<T>();
    let layout = unsafe { Layout::from_size_align_unchecked(total_size, align) };
    (allocation, layout)
}

impl<T> RcVec<T> {
    pub(crate) fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub(crate) fn len(&self) -> usize {
        self.header().len
    }

    pub(crate) fn iter(&self) -> slice::Iter<T> {
        let len = self.len();
        unsafe { slice::from_raw_parts(self.first, len) }.iter()
    }

    pub(crate) fn make_mut(&mut self) -> RcVecMut<T>
    where
        T: Clone,
    {
        let header = self.header();
        let refcount = header.refcount.get();
        if refcount > 1 {
            let mut builder = RcVecBuilder::new();
            builder.extend(self.iter().cloned());
            // NOTE: assumes that none of the clones on the previous line
            // resulted in self's refcount changing. The public API of RcVec
            // does not expose any way to form cyclic structures.
            header.refcount.set(refcount - 1);
            self.first = builder.first;
            mem::forget(builder);
        }
        RcVecMut {
            first: &mut self.first,
        }
    }

    pub(crate) fn get_mut(&mut self) -> Option<RcVecMut<T>> {
        if self.header().refcount.get() > 1 {
            None
        } else {
            Some(RcVecMut {
                first: &mut self.first,
            })
        }
    }

    pub(crate) fn make_owned(self) -> RcVecBuilder<T>
    where
        T: Clone,
    {
        let header = self.header();
        let refcount = header.refcount.get();
        let first;
        if refcount > 1 {
            let mut builder = RcVecBuilder::new();
            builder.extend(self.iter().cloned());
            header.refcount.set(refcount - 1);
            first = builder.first;
            mem::forget(builder);
        } else {
            first = self.first;
        }
        mem::forget(self);
        RcVecBuilder { first }
    }

    fn header(&self) -> &Header {
        unsafe { header(self.first) }
    }
}

impl<T> RcVecBuilder<T> {
    pub(crate) fn new() -> Self {
        Self::with_capacity(8)
    }

    pub(crate) fn with_capacity(cap: usize) -> Self {
        assert!(mem::size_of::<T>() > 0);

        // Alignment of the allocation.
        let align = cmp::max(mem::align_of::<T>(), mem::align_of::<Header>());

        // Round up Header's size to a multiple of allocation's alignment.
        let mut header_size = mem::size_of::<Header>();
        header_size = (header_size + align - 1) / align * align;

        let total_size = mem::size_of::<T>()
            .saturating_mul(cap)
            .saturating_add(header_size);
        let layout = match Layout::from_size_align(total_size, align) {
            Ok(layout) => layout,
            Err(_) => process::abort(),
        };

        unsafe {
            let ptr = alloc::alloc(layout);
            let first = ptr.add(header_size) as *mut T;
            (first as *mut Header).offset(-1).write(Header {
                cap,
                len: 0,
                refcount: Cell::new(1),
            });
            RcVecBuilder { first }
        }
    }

    pub(crate) fn push(&mut self, element: T) {
        self.as_mut().push(element);
    }

    pub(crate) fn extend(&mut self, iter: impl IntoIterator<Item = T>) {
        self.as_mut().extend(iter);
    }

    pub(crate) fn as_mut(&mut self) -> RcVecMut<T> {
        RcVecMut {
            first: &mut self.first,
        }
    }

    pub(crate) fn build(self) -> RcVec<T> {
        let first = self.first;
        mem::forget(self);
        RcVec { first }
    }
}

impl<'a, T> RcVecMut<'a, T> {
    pub(crate) fn push(&mut self, element: T) {
        self.reserve(1);
        let header = unsafe { header_mut(*self.first) };
        unsafe { ptr::write(self.first.add(header.len), element) };
        header.len += 1;
    }

    pub(crate) fn extend(&mut self, elements: impl IntoIterator<Item = T>) {
        let iter = elements.into_iter();
        self.reserve(iter.size_hint().0);
        iter.for_each(|element| self.push(element));
    }

    pub(crate) fn as_mut(&mut self) -> RcVecMut<T> {
        RcVecMut { first: self.first }
    }

    pub(crate) fn take(self) -> RcVecBuilder<T> {
        let mut builder = RcVecBuilder::new();
        mem::swap(self.first, &mut builder.first);
        builder
    }

    fn reserve(&mut self, additional: usize) {
        let header = unsafe { header(*self.first) };
        let required_capacity = header.len.saturating_add(additional);
        if header.cap >= required_capacity {
            return;
        }

        let min_new_capacity = cmp::max(8, header.cap.saturating_mul(2));
        let new_capacity = cmp::max(required_capacity, min_new_capacity);

        // Move ownership of the old allocation.
        let small = RcVecBuilder { first: *self.first };

        // Move elements into new allocation.
        let mut big = RcVecBuilder::with_capacity(new_capacity);
        big.extend(small);

        // Self becomes big.
        *self.first = big.first;
        mem::forget(big);
    }
}

impl<T> Clone for RcVec<T> {
    fn clone(&self) -> Self {
        let header = self.header();
        let count = header.refcount.get().checked_add(1).unwrap();
        header.refcount.set(count);
        RcVec { first: self.first }
    }
}

impl<T> IntoIterator for RcVecBuilder<T> {
    type Item = T;
    type IntoIter = RcVecIntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        let first = self.first;
        mem::forget(self);
        RcVecIntoIter { first, cur: first }
    }
}

impl<T> Iterator for RcVecIntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            let len = header(self.first).len;
            if self.cur < self.first.add(len) {
                let element = ptr::read(self.cur);
                self.cur = self.cur.add(1);
                Some(element)
            } else {
                None
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let end = unsafe { self.first.add(header(self.first).len) };
        let remaining = (end as usize - self.cur as usize) / mem::size_of::<T>();
        (remaining, Some(remaining))
    }
}

impl<T> Drop for RcVec<T> {
    fn drop(&mut self) {
        let header = self.header();
        let refcount = header.refcount.get();
        if refcount > 1 {
            header.refcount.set(refcount - 1);
        } else {
            drop(RcVecBuilder { first: self.first });
        }
    }
}

impl<T> Drop for RcVecBuilder<T> {
    fn drop(&mut self) {
        unsafe {
            let len = header(self.first).len;
            let slice = slice::from_raw_parts_mut(self.first, len);
            ptr::drop_in_place(slice);
            let (ptr, layout) = allocation_layout(self.first);
            alloc::dealloc(ptr, layout);
        }
    }
}

impl<T> Drop for RcVecIntoIter<T> {
    fn drop(&mut self) {
        unsafe {
            let len = header(self.first).len;
            let offset = (self.cur as usize - self.first as usize) / mem::size_of::<T>();
            let slice = slice::from_raw_parts_mut(self.cur, len - offset);
            ptr::drop_in_place(slice);
            let (ptr, layout) = allocation_layout(self.first);
            alloc::dealloc(ptr, layout);
        }
    }
}

impl<T> RefUnwindSafe for RcVec<T> where T: RefUnwindSafe {}
