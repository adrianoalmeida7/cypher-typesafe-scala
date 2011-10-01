package com.ahalmeida7.lucascs

import org.neo4j.cypher.commands._

class Cypher
  extends StartClause
  with WhereClause
  with OrderByClause
  with ReturnClause
  with Pagination {

  def toQuery = {
    implicit def elvis(b:QueryBuilder) = new {
      def opt[U](x:Option[U])(fun:(QueryBuilder, U) => QueryBuilder) = x.map(fun(b, _)).getOrElse(b)
      def opt[U](x:Seq[U])(fun:(QueryBuilder, Seq[U]) => QueryBuilder) = if (x.isEmpty) b else fun(b,x)
    }
    Query.start(startNode:_*)
      .opt(whereClause)(_.where(_))
      .opt(orderBys)(_.orderBy(_:_*))
      .opt(limitValue)(_.limit(_))
      .opt(skipValue)(_.skip(_))
      .returns(returnItem:_*)
  }

}