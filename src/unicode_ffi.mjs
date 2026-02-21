// Unicode ID_Start: \p{L} \p{Nl} + Other_ID_Start
const ID_START_RE = /^\p{ID_Start}$/u;
// Unicode ID_Continue: ID_Start + \p{Mn} \p{Mc} \p{Nd} \p{Pc} + Other_ID_Continue
const ID_CONTINUE_RE = /^\p{ID_Continue}$/u;

export function is_id_start(cp) {
  return ID_START_RE.test(String.fromCodePoint(cp));
}

export function is_id_continue(cp) {
  return ID_CONTINUE_RE.test(String.fromCodePoint(cp));
}
