package com.ahalmeida7.lucascs

import org.neo4j.cypher.commands._

class Cypher {
  var startNode:Seq[StartItem] = _

  var whereClause:Option[Clause] = None

  var returnItem:Seq[ReturnItem] = _

  implicit def symbol2fun(identifier:Symbol) = new {
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

  implicit def str2fun(str:String) = new {
    def === (other:Any) = {
      val pieces = str.split("\\.")
      Equals(PropertyValue(pieces(0), pieces(1)), Literal(other))
    }
  }


  def start(x:StartItem*) { startNode = x }

  def where(x:Clause) {
    whereClause = Some(x)
  }

  def toQuery = {
    implicit def elvis(b:QueryBuilder) = new {
      def opt[U](x:Option[U])(fun:(QueryBuilder, U) => QueryBuilder) = x.map(fun(b, _)).getOrElse(b)
    }
    Query.start(startNode:_*)
      .opt(whereClause)(_.where(_))
      .returns(returnItem:_*)
  }

  def returns(identifier:Symbol*) {
    returnItem = identifier.map(i => ValueReturnItem(EntityValue(i.name)))
  }
}

trait Node {

}

case class StartNode(id:Long) extends Node