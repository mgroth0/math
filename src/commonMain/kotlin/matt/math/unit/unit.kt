package matt.math.unit

enum class UnitType(val symbol: String?, val longNameSingular: String?, val longNamePlural: String?) {
  PERCENT("%", "matt.model.percent.getPercent", "matt.model.percent.getPercent"),
  DEGREES("°", "degree", "degrees"),
  RATIO(null, null, null)
}