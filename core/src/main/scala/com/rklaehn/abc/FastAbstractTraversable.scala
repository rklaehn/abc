//package com.rklaehn.abc
//
//import scala.collection.{mutable, Traversable, AbstractTraversable}
//import scala.collection.generic.TraversableFactory
//import scala.collection.mutable.Builder
//
//object FastTraversable {
//
//  def fromForeach[T, U, V](injectedForeach:(T=>U)=>V): Traversable[T] =
//    new FastTraversable[T, U, V](injectedForeach)
//
//  def fromForeach0[T, U, V](injectedForeach: (T => U) => V) = new Traversable[T] {
//    def foreach[W](f: (T) => W) {
//      injectedForeach(e => {
//        f(e)
//        null.asInstanceOf[U]
//      })
//    }
//  }
//
//  private class FastTraversable[T, U, V](injectedForeach: (T => U) => V) extends AbstractTraversable[T] {
//    def foreach[W](f: (T) => W) {
//      injectedForeach(e => {
//        f(e)
//        null.asInstanceOf[U]
//      })
//    }
//
//    override def companion = FastTraversable.factory
//  }
//
//  private val factory = new TraversableFactory[Traversable] {
//
//    def newBuilder[A]: Builder[A, Traversable[A]] = new mutable.ArrayBuffer[A]
//  }
//}
