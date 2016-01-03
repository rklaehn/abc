package com.rklaehn.abc

import algebra.Order

/**
  * Simple implementation of insertion sort.
  *
  * Works for small arrays but due to O(n^2) complexity is not generally good.
  */
private object InsertionSort {

  final def sort[@sp A](data:Array[A], start:Int, end:Int)(implicit o:Order[A]): Unit = {

    var i = start + 1
    while (i < end) {
      val item = data(i)
      var hole = i
      while (hole > start && o.gt(data(hole - 1), item)) {
        data(hole) = data(hole - 1)
        hole -= 1
      }
      data(hole) = item
      i += 1
    }
  }
}

/**
  * In-place quicksort implementation. It is not stable, but does not allocate
  * extra space (other than stack). Like MergeSort, it uses InsertionSort for
  * sorting very small arrays.
  */
private object QuickSort {
  @inline final def limit: Int = 16

  final def sort[@sp A:Order](data:Array[A]): Unit = qsort(data, 0, data.sl - 1)

  final def qsort[@sp A](data:Array[A], left: Int, right: Int)(implicit o:Order[A]): Unit = {

    if (right - left < limit)
      InsertionSort.sort(data, left, right + 1)
    else {
      val pivot = left + (right - left) / 2
      val next = partition(data, left, right, pivot)
      qsort(data, left, next - 1)
      qsort(data, next + 1, right)
    }
  }

  final def partition[@sp A](data:Array[A], left:Int, right:Int, pivot:Int)(implicit o:Order[A]): Int = {

    val value = data(pivot)

    //swap(pivot, right)
    var tmp = data(pivot); data(pivot) = data(right); data(right) = tmp

    var store = left
    var i = left
    while (i < right) {
      if (o.lt(data(i), value)) {
        //swap(i, store)
        tmp = data(i); data(i) = data(store); data(store) = tmp
        store += 1
      }
      i += 1
    }
    //swap(store, right)
    tmp = data(store); data(store) = data(right); data(right) = tmp
    store
  }
}

/**
  * Object providing in-place sorting capability for arrays.
  *
  * Sorting.sort() uses quickSort() by default (in-place, not stable, generally
  * fastest but might hit bad cases where it's O(n^2)). Also provides
  * mergeSort() (in-place, stable, uses extra memory, still pretty fast) and
  * insertionSort(), which is slow except for small arrays.
  */
private object Sorting {
  final def sort[@sp A:Order](data:Array[A]): Unit = QuickSort.sort(data)
}
