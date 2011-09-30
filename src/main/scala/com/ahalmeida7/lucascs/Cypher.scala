package com.ahalmeida7.lucascs

import collection.mutable.Map
import org.neo4j.cypher.commands._

class Cypher {
  var startNode:Seq[StartItem] = _

  var returnItem:ReturnItem = _

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

  def start(x:StartItem*) { startNode = x }

  def toQuery = {
    Query.start(startNode:_*)
      .returns(returnItem)
  }

  def returns(identifier:Symbol) {
    returnItem = ValueReturnItem(EntityValue(identifier.name))
  }
}

trait Node {

}

case class StartNode(id:Long) extends Node