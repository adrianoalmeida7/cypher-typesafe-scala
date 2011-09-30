package com.ahalmeida7.lucascs

import org.neo4j.cypher.commands._
import util.matching.Regex

class Cypher {
  var startNode:Seq[StartItem] = _

  var whereClause:Option[Clause] = None
  var orderBys:Seq[SortItem] = Seq()

  var returnItem:Seq[ReturnItem] = _

  var limitValue:Option[Int] = None
  var skipValue:Option[Int] = None

  implicit def symbol2starts(identifier:Symbol) = new {
    def :=(ids:Long*) = {
      NodeById(identifier.name, ids:_*)
    }
    def :=(idx:Symbol, key:Symbol, value:Any) = {
      NodeByIndex(identifier.name, idx.name, key.name, value)
    }
    def :=(idx:Symbol, keyValue:String) = {
      NodeByIndexQuery(identifier.name, idx.name, keyValue)
    }
    def :=< (id:Long*) = {
      RelationshipById(identifier.name, id:_*)
    }
    def :=<(idx:Symbol, key:Symbol, value:Any) = {
      RelationshipByIndex(identifier.name, idx.name, key.name, value)
    }
  }

  implicit def str2clauses(str:(Symbol, Symbol)) = new {
    def ===(other:Any) = {
      Equals(PropertyValue(str._1.name, str._2.name), Literal(other))
    }
    def < (other:Any) = {
      LessThan(PropertyValue(str._1.name, str._2.name), Literal(other))
    }
    def <= (other:Any) = {
      LessThanOrEqual(PropertyValue(str._1.name, str._2.name), Literal(other))
    }
    def > (other:Any) = {
      GreaterThan(PropertyValue(str._1.name, str._2.name), Literal(other))
    }
    def >= (other:Any) = {
      GreaterThanOrEqual(PropertyValue(str._1.name, str._2.name), Literal(other))
    }
    def <> (other:Any) = {
      Not(===(other))
    }
    def =~ (other:Regex):Clause = {
      =~(other.toString())
    }
    def =~ (other:String):Clause = {
      RegularExpression(PropertyValue(str._1.name, str._2.name), other)
    }
  }
  implicit def clause2logic(c:Clause) = new {
    def && (other:Clause) = And(c, other)
    def || (other:Clause) = Or(c, other)
    def unary_! = Not(c)
  }

  implicit def symbols2Order(prop:(Symbol, Symbol)) = SortItem(ValueReturnItem(PropertyValue(prop._1.name, prop._2.name)), true)
  implicit def symbols2ascdesc(prop:(Symbol, Symbol)) = new {
    def asc:SortItem = prop

    def desc:SortItem = SortItem(ValueReturnItem(PropertyValue(prop._1.name, prop._2.name)), false)
  }

  def start(x:StartItem*) { startNode = x }

  def where(x:Clause) {
    whereClause = Some(x)
  }

  def orderBy(props:SortItem*) {
    orderBys = props
  }

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

  def returns(identifier:Symbol*) {
    returnItem = identifier.map(i => ValueReturnItem(EntityValue(i.name)))
  }

  def limit(num:Int) {
    limitValue = Some(num)
  }
  def skip(num:Int) {
    skipValue = Some(num)
  }
}

trait Node {

}

case class StartNode(id:Long) extends Node