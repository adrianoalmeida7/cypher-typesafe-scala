package com.ahalmeida7.lucascs

import org.scalatest.FlatSpec
import org.neo4j.cypher.javacompat.CypherParser
import org.scalatest.matchers.ShouldMatchers

class CypherTest extends FlatSpec with ShouldMatchers {

  "Cypher" should "compile the simplest query" in {
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

  it should "compile multiple starts and returns" in {
     val query = new Cypher {
       start('a := 1, 'b := 2)

       returns('a, 'b)
     }
    val parser = new CypherParser()

    query.toQuery should be === parser.parse("start a = (1), b = (2) return a, b")
  }

  it should "filter on a property" in {
     val query = new Cypher {
       start('a := 1)
       where('a->'name === "adriano")
       returns('a)
     }
    val parser = new CypherParser()

    query.toQuery should be === parser.parse("start a = (1) where a.name = \"adriano\" return a")
  }

  it should "filter on a property with logical connectors" in {
     val query = new Cypher {
       start('a := 1)
       where('a->'name === "adriano" || !('a->'age > 13))
       returns('a)
     }
    val parser = new CypherParser()

    query.toQuery should be === parser.parse("start a = (1) where a.name = \"adriano\" or not (a.age > 13) return a")
  }

  it should "handler regular expressions" in {
     val query = new Cypher {
       start('a := 1)
       where('a->'name =~ "adr.*".r)
       returns('a)
     }
    val parser = new CypherParser()

    query.toQuery should be === parser.parse("start a = (1) where a.name =~ /adr.*/ return a")
  }

  it should "compile order by clause" in {
     val query = new Cypher {
       start('a := 1)
       returns('a)
       orderBy('a->'name)
     }
    val parser = new CypherParser()

    query.toQuery should be === parser.parse("start a = (1) return a order by a.name")
  }
  it should "compile order by clause with many props" in {
     val query = new Cypher {
       start('a := 1)
       returns('a)
       orderBy('a->'name asc, 'a->'age desc)
     }
    val parser = new CypherParser()

    query.toQuery should be === parser.parse("start a = (1) return a order by a.name asc, a.age desc")
  }
  it should "compile limits" in {
     val query = new Cypher {
       start('a := 1)

       returns('a)

       limit(1)
     }
    val parser = new CypherParser()

    query.toQuery should be === parser.parse("start a = (1) return a limit 1")
  }
  it should "compile skips" in {
     val query = new Cypher {
       start('a := 1)

       returns('a)

       skip(1)
     }
    val parser = new CypherParser()

    query.toQuery should be === parser.parse("start a = (1) return a skip 1")
  }
}