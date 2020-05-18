package endpoints.generic

import endpoints.algebra

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

/**
  * Enriches [[JsonSchemas]] with two kinds of operations:
  *
  *   - `genericJsonSchema[A]` derives the `JsonSchema` of an algebraic
  *     data type `A`;
  *   - `(field1 :×: field2 :×: …).as[A]` builds a tuple of `Record`s and maps
  *     it to a case class `A`
  *
  * For instance, consider the following program that derives the JSON schema
  * of a case class:
  *
  * {{{
  *   case class User(name: String, age: Int)
  *   object User {
  *     implicit val schema: JsonSchema[User] = genericJsonSchema[User]
  *   }
  * }}}
  *
  * It is equivalent to the following:
  *
  * {{{
  *   case class User(name: String, age: Int)
  *   object User {
  *     implicit val schema: JsonSchema[User] = (
  *       field[String]("name") zip
  *       field[Int]("age")
  *     ).xmap((User.apply _).tupled)(Function.unlift(User.unapply))
  *   }
  * }}}
  *
  */
trait JsonSchemas extends algebra.JsonSchemas {

  @implicitNotFound(
    "Unable to derive an instance of JsonSchema[${A}].\n" +
      "The type ${A} must be a sealed trait or a case class.\n" +
      "If it is a sealed trait, make sure it is only extended by case classes and that a " +
      "JsonSchema can be derived for each of these case classes (see hereafter).\n" +
      "If it is a case class, make sure each field of the case class has an implicit JsonSchema.\n" +
      "You can check that a JsonSchema is available for a type T by compiling the expression " +
      "`implicitly[JsonSchema[T]]`."
  )
  trait GenericJsonSchema[A] {
    def jsonSchema: JsonSchema[A]
  }

  object GenericJsonSchema {

    @implicitNotFound(
      "Unable to derive an instance of JsonSchema[${A}].\n" +
        "The type ${A} must be a case class. Make sure each field of the case class has an implicit JsonSchema.\n" +
        "You can check that a JsonSchema is available for a type T by compiling the expression " +
        "`implicitly[JsonSchema[T]]`.\n" +
        "If type ${A} is a sealed trait, use `genericTagged` or `genericJsonSchema` instead."
    )
    class GenericRecord[A](val jsonSchema: Record[A])
      extends GenericJsonSchema[A]

    @implicitNotFound(
      "Unable to derive an instance of JsonSchema[${A}].\n" +
        "The type ${A} must be a sealed trait. Make sure it is only extended by case classes, and " +
        "for each case class, make sure each field of the case class has an implicit JsonSchema.\n" +
        "You can check that a JsonSchema is available for a type T by compiling the expression " +
        "`implicitly[JsonSchema[T]]`.\n" +
        "If type ${A} is a case class, use `genericRecord` or `genericJsonSchema` instead."
    )
    class GenericTagged[A](val jsonSchema: Tagged[A])
      extends GenericJsonSchema[A]



  }

  /**
    * Compute a schema name (used for documentation) based on a `ClassTag`.
    * The provided implementation uses the fully qualified name of the class.
    * This could result in non unique values and mess with documentation.
    *
    * You can override this method to use a custom logic.
    */
  def classTagToSchemaName(ct: ClassTag[_]): String = {
    val jvmName = ct.runtimeClass.getName
    // name fix for case objects
    val name =
      if (jvmName.nonEmpty && jvmName.last == '$') jvmName.init else jvmName
    name.replace('$', '.')
  }

  /** Derives a `JsonSchema[A]` for a type `A`.
    *
    * In a sense, this operation asks shapeless to compute a ''type level'' description
    * of a data type (based on HLists and Coproducts) and turns it into a ''term level''
    * description of the data type (based on the `JsonSchemas` algebra interface)
    *
    * @see [[genericRecord]] for details on how schemas are derived for case classes
    * @see [[genericTagged]] for details on how schemas are derived for sealed traits
    */
  def genericJsonSchema[A](
      using genJsonSchema: GenericJsonSchema[A]
  ): JsonSchema[A] =
    genJsonSchema.jsonSchema

  /** Derives a `Record[A]` schema for a case class `A`.
    *
    * The resulting schema:
    *
    *   - describes a JSON object,
    *   - has required properties of the same name and type as each case class field,
    *   - has optional properties of the same name and type as each case class
    *     field of type `Option[X]` for some type `X`,
    *   - includes the description possibly attached to each case class field
    *     via the [[docs @docs]] annotation,
    *   - has a name, computed from a `ClassTag[A]` by the [[classTagToSchemaName]]
    *     operation.
    */
  def genericRecord[A](
      using genRecord: GenericJsonSchema.GenericRecord[A]
  ): Record[A] =
    genRecord.jsonSchema

  /** Derives a `Tagged[A]` schema for a sealed trait `A`.
    *
    * The resulting schema:
    *
    *   - is the alternative of the leaf case classes schemas,
    *   - the field used for discriminating the alternatives is defined by the
    *     [[discriminator @discriminator]] annotation, if present on the sealed
    *     trait definition, or by the [[defaultDiscriminatorName]] method otherwise,
    *   - each alternative is discriminated by the name (not qualified) of the
    *     case class.
    */
  def genericTagged[A](
      using genTagged: GenericJsonSchema.GenericTagged[A]
  ): Tagged[A] =
    genTagged.jsonSchema

//  extension recordGenericOps on [A](record: RecordA) {
//    def as[B](using gen: Generic.Aux[A, B]): Record[B] =
//      record.xmap(gen.to)(gen.from)
//  }

//  extension jsonSchemaGenericOps on [A](schema: JsonSchema[A]) {
//    def as[B](using gen: Generic.Aux[A, B]): JsonSchema[B] =
//      schema.xmap(gen.to)(gen.from)
//  }

}
