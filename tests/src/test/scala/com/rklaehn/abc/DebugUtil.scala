package com.rklaehn.abc

object DebugUtil {

  implicit class IsSpecializedExtension(private val ref: AnyRef) extends AnyVal {

    def isIntArray: Boolean = (ref eq null) || ref.getClass.toString.endsWith("[I")

    def isSpecialized: Boolean = ref.getClass.getName.endsWith("$sp")
  }
}
