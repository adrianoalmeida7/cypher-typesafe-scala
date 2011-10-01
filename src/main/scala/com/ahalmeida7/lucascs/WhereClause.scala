package com.ahalmeida7.lucascs

import util.matching.Regex
import org.neo4j.cypher.commands._

trait WhereClause {

  var whereClause:Option[Clause] = None

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

  def where(x:Clause) {
    whereClause = Some(x)
  }

}