package com.rklaehn.abc

import ichi.bench.Thyme
import ichi.bench.Thyme.HowWarm

object DebugUtil {

  // val th = Thyme.warmed(verbose = println, warmth = HowWarm.BenchOff)
  val th = new Thyme

  implicit class IsSpecializedExtension(private val ref: AnyRef) extends AnyVal {

    def isIntArray: Boolean = ref.getClass.toString.endsWith("[I")

    def isSpecialized: Boolean = ref.getClass.getName.endsWith("$sp")
  }
}
