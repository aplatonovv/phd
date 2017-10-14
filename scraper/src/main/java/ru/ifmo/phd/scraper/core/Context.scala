package ru.ifmo.phd.scraper.core

trait Context {
  def append(field: String, value: Context): Context

  def set(field: String, value: Context): Context

  def toJson(): String

  override def toString: String = toJson()
}

case class StringContext(value: String) extends Context {
  override def append(field: String, value: Context): Context = StringContext {
    val kv = s"$field: $value"

    if(this.value.isEmpty) kv
    else s"$value; $kv"
  }

  override def set(field: String, value: Context): Context = StringContext(s"$field: $value")

  override def toJson(): String = value
}

case class NestedContext(value: Map[String, List[Context]]) extends Context {
  override def append(field: String, value: Context): Context = set(
    field,
    value :: this.value.getOrElse(field, Nil)
  )

  override def set(field: String, value: Context): Context = set(field, value :: Nil)

  private def set(field: String, value: List[Context]) = NestedContext(this.value + (field -> value))

  override def toJson(): String = value.map({
    case (key, value) => s"$key: $value"
  }).mkString("{", ", ", "}")
}
