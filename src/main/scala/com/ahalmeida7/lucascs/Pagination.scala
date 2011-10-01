package com.ahalmeida7.lucascs

trait Pagination {
  var limitValue:Option[Int] = None
  var skipValue:Option[Int] = None

  def limit(num:Int) {
    limitValue = Some(num)
  }
  def skip(num:Int) {
    skipValue = Some(num)
  }
}