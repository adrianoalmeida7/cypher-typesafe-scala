package com.ahalmeida7.lucascs

import org.neo4j.cypher.commands.StartItem
import org.neo4j.cypher.commands.NodeById
import org.neo4j.cypher.commands.NodeByIndex
import org.neo4j.cypher.commands.NodeByIndexQuery
import org.neo4j.cypher.commands.RelationshipById
import org.neo4j.cypher.commands.RelationshipByIndex

trait StartClause {
   var startNode:Seq[StartItem] = _

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

  def start(x:StartItem*) { startNode = x }
}