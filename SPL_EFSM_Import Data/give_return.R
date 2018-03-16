give_return = function(object) {
  object = object %>%
    ts(frequency = 1) %>%
    Return.calculate() %>%
    round(digits = 4)
  return(object)
}