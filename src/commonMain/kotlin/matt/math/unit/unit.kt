package matt.math.unit

enum class UnitType(val symbol: String?, val longNameSingular: String?, val longNamePlural: String?) {
  PERCENT("%", "percent", "percent"),
  DEGREES("°", "degree", "degrees"),
  RATIO(null, null, null)
}