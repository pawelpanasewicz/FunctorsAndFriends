package ynfrastructure

import org.scalatest._
import org.scalatest.prop.PropertyChecks

class   Spec
extends FreeSpec
with    MustMatchers
with    DiagrammedAssertions
with    PropertyChecks
with    TryValues
with    OptionValues
with    AppendedClues {

  def theSameType[A,B](implicit ev: A =:= B) = ()
  /***
    * Witness that A and B have the same parent type of SuperType
    */
  def theSameParentType[A <: SuperType, B <: SuperType, SuperType] = ()
  def subType[A, B](implicit ev: A <:< B) = ()
}