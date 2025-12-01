fn int print_is_app(Term t) {
  return term_tag(t) == APP || (term_tag(t) == C02 && term_ext(t) == _APP_);
}
