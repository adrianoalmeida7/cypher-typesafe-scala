package com.ahalmeida7.lucascs

import org.scalatest.FlatSpec
import org.neo4j.cypher.javacompat.CypherParser
import org.scalatest.matchers.ShouldMatchers

class CypherTest extends FlatSpec with ShouldMatchers {

  "Cypher" should "compile the most dumb query evah" in {
     val query = new Cypher {
       start('s := (3,4,5,6))

       returns('s)
     }
    val parser = new CypherParser()

    query.toQuery should be === parser.parse("start s = (3, 4, 5, 6) return s")
  }
  it should "compile key queries" in {
     val query = new Cypher {
       start('a := ('index, 'key, "value"))

       returns('a)
     }
    val parser = new CypherParser()

    query.toQuery should be === parser.parse("start a = (index, key, \"value\") return a")
  }

  it should "compile key index queries" in {
     val query = new Cypher {
       start('a := ('index, "key:value"))

       returns('a)
     }
    val parser = new CypherParser()

    query.toQuery should be === parser.parse("start a = (index, \"key:value\") return a")
  }

  it should "compile relationships" in {
     val query = new Cypher {
       start('a :=< (1,2,3))

       returns('a)
     }
    val parser = new CypherParser()

    query.toQuery should be === parser.parse("start a = <1,2,3> return a")
  }

  it should "compile relationships by index" in {
     val query = new Cypher {
       start('a :=< ('index, 'key, "value"))

       returns('a)
     }
    val parser = new CypherParser()

    query.toQuery should be === parser.parse("start a = <index, key, \"value\"> return a")
  }
}