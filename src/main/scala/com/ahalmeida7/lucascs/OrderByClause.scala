package com.ahalmeida7.lucascs

import org.neo4j.cypher.commands._

trait OrderByClause {

  var orderBys:Seq[SortItem] = Seq()

  implicit def symbols2Order(prop:(Symbol, Symbol)) = SortItem(ValueReturnItem(PropertyValue(prop._1.name, prop._2.name)), true)
  implicit def symbols2ascdesc(prop:(Symbol, Symbol)) = new {
    def asc:SortItem = prop

    def desc:SortItem = SortItem(ValueReturnItem(PropertyValue(prop._1.name, prop._2.name)), false)
  }

  def orderBy(props:SortItem*) {
    orderBys = props
  }
}