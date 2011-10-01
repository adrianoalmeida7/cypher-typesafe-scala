package com.ahalmeida7.lucascs

import org.neo4j.cypher.commands._

trait ReturnClause {

  var returnItem:Seq[ReturnItem] = _

  def returns(identifier:Symbol*) {
    returnItem = identifier.map(i => ValueReturnItem(EntityValue(i.name)))
  }

}