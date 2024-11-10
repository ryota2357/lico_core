use super::*;

pub(super) const PAT_FIRST: TokenSet = TokenSet::new(&[IDENT, T![_]]);

pub(super) fn pat(p: &mut Parser) {
    assert!(p.at_ts(PAT_FIRST));

    // SAFETY: `p.at_ts(..)` is true, so `p.current()` is not None.
    match unsafe { p.current().unwrap_unchecked() } {
        IDENT => name(p),
        T![_] => wildcard_pat(p),
        _ => unreachable!(),
    }
}

fn wildcard_pat(p: &mut Parser) {
    let m = p.start();
    p.bump(T![_]);
    m.complete(p, WILDCARD_PAT);
}
