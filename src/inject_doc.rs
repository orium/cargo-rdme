/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::{Doc, Readme};
use thiserror::Error;

const MARKER_RDME: &str = "<!-- cargo-rdme -->";
const MARKER_RDME_START: &str = "<!-- cargo-rdme start -->";
const MARKER_RDME_END: &str = "<!-- cargo-rdme end -->";

#[derive(PartialEq, Eq, Debug)]
enum CargoRdmeLine<'a> {
    Line(&'a str),
    MarkerCargoRdme,
    MarkerCargoRdmeStart,
    MarkerCargoRdmeEnd,
}

fn cargo_rdme_line_iterator(readme: &Readme) -> impl Iterator<Item = CargoRdmeLine<'_>> {
    let mut depth = 0;

    readme.lines().map(move |line| {
        let trimmed_line = line.strip_suffix("\r").unwrap_or(line).trim();

        match trimmed_line {
            MARKER_RDME if depth == 0 => CargoRdmeLine::MarkerCargoRdme,
            MARKER_RDME_START if depth == 0 => {
                depth += 1;
                CargoRdmeLine::MarkerCargoRdmeStart
            }
            MARKER_RDME_END if depth == 1 => {
                depth -= 1;
                CargoRdmeLine::MarkerCargoRdmeEnd
            }
            MARKER_RDME_START => {
                depth += 1;
                CargoRdmeLine::Line(line)
            }
            MARKER_RDME_END => {
                depth -= 1;
                CargoRdmeLine::Line(line)
            }
            _ => CargoRdmeLine::Line(line),
        }
    })
}

#[derive(Error, Debug)]
pub enum InjectDocError {
    #[error("unexpected end marker at line {line_number}")]
    UnexpectedMarkerCargoRdmeEnd { line_number: usize },
    #[error("unmatched start marker")]
    UnmatchedMarkerCargoRdmeStart,
}

pub fn inject_doc(readme: &Readme, doc: &Doc) -> Result<Readme, InjectDocError> {
    let mut new_readme: Vec<String> = Vec::with_capacity(1024);
    let mut inside_markers = false;

    fn inject(new_readme: &mut Vec<String>, doc: &Doc) {
        new_readme.push(MARKER_RDME_START.to_owned());
        new_readme.push("".to_owned());
        new_readme.extend(doc.lines().map(ToOwned::to_owned));
        new_readme.push("".to_owned());
        new_readme.push(MARKER_RDME_END.to_owned());
    }

    for (i, line) in cargo_rdme_line_iterator(&readme).enumerate() {
        match (inside_markers, line) {
            (true, CargoRdmeLine::MarkerCargoRdmeEnd) => {
                inside_markers = false;
            }
            (true, _) => (),

            (false, CargoRdmeLine::MarkerCargoRdmeEnd) => {
                return Err(InjectDocError::UnexpectedMarkerCargoRdmeEnd { line_number: i + 1 });
            }
            (false, CargoRdmeLine::Line(s)) => new_readme.push(s.to_owned()),
            (false, CargoRdmeLine::MarkerCargoRdme) => {
                inject(&mut new_readme, doc);
            }
            (false, CargoRdmeLine::MarkerCargoRdmeStart) => {
                inject(&mut new_readme, doc);
                inside_markers = true;
            }
        }
    }

    match inside_markers {
        true => Err(InjectDocError::UnmatchedMarkerCargoRdmeStart),
        false => Ok(Readme::from_lines(&new_readme)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use itertools::Itertools;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_cargo_rdme_line_iterator() {
        let str = indoc! { "
            marker test <!-- cargo-rdme -->.
             Starting with whitespace.

            <!-- cargo-rdme start -->
            <!-- cargo-rdme end -->
            <!-- cargo-rdme -->
            <!-- cargo-rdme end --> <- Does not count.
             <!-- cargo-rdme start -->
            <!-- cargo-rdme end -->\r
            <!-- cargo-rdme start -->
            <!-- cargo-rdme end --> \r
            <!-- cargo-rdme start -->
            <!-- cargo-rdme end --> "
        };

        let readme = Readme::from_str(str);
        let mut iter = cargo_rdme_line_iterator(&readme);

        assert_eq!(iter.next(), Some(CargoRdmeLine::Line("marker test <!-- cargo-rdme -->.")));
        assert_eq!(iter.next(), Some(CargoRdmeLine::Line(" Starting with whitespace.")));
        assert_eq!(iter.next(), Some(CargoRdmeLine::Line("")));
        assert_eq!(iter.next(), Some(CargoRdmeLine::MarkerCargoRdmeStart));
        assert_eq!(iter.next(), Some(CargoRdmeLine::MarkerCargoRdmeEnd));
        assert_eq!(iter.next(), Some(CargoRdmeLine::MarkerCargoRdme));
        assert_eq!(
            iter.next(),
            Some(CargoRdmeLine::Line("<!-- cargo-rdme end --> <- Does not count."))
        );
        assert_eq!(iter.next(), Some(CargoRdmeLine::MarkerCargoRdmeStart));
        assert_eq!(iter.next(), Some(CargoRdmeLine::MarkerCargoRdmeEnd));
        assert_eq!(iter.next(), Some(CargoRdmeLine::MarkerCargoRdmeStart));
        assert_eq!(iter.next(), Some(CargoRdmeLine::MarkerCargoRdmeEnd));
        assert_eq!(iter.next(), Some(CargoRdmeLine::MarkerCargoRdmeStart));
        assert_eq!(iter.next(), Some(CargoRdmeLine::MarkerCargoRdmeEnd));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_cargo_rdme_line_iterator_nested() {
        let str = indoc! { "
            A
            <!-- cargo-rdme start -->
            B
            <!-- cargo-rdme -->
            C
            <!-- cargo-rdme start -->
            D
            <!-- cargo-rdme end -->
            E
            <!-- cargo-rdme end -->
            F"
        };

        let readme = Readme::from_str(str);
        let mut iter = cargo_rdme_line_iterator(&readme);

        assert_eq!(iter.next(), Some(CargoRdmeLine::Line("A")));
        assert_eq!(iter.next(), Some(CargoRdmeLine::MarkerCargoRdmeStart));
        assert_eq!(iter.next(), Some(CargoRdmeLine::Line("B")));
        assert_eq!(iter.next(), Some(CargoRdmeLine::Line("<!-- cargo-rdme -->")));
        assert_eq!(iter.next(), Some(CargoRdmeLine::Line("C")));
        assert_eq!(iter.next(), Some(CargoRdmeLine::Line("<!-- cargo-rdme start -->")));
        assert_eq!(iter.next(), Some(CargoRdmeLine::Line("D")));
        assert_eq!(iter.next(), Some(CargoRdmeLine::Line("<!-- cargo-rdme end -->")));
        assert_eq!(iter.next(), Some(CargoRdmeLine::Line("E")));
        assert_eq!(iter.next(), Some(CargoRdmeLine::MarkerCargoRdmeEnd));
        assert_eq!(iter.next(), Some(CargoRdmeLine::Line("F")));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_inject_doc_single_marker() {
        let readme_str = indoc! { r#"
            This is a really nice crate.

            <!-- cargo-rdme -->

            Hope you enjoy!
            "#
        };
        let doc_str = indoc! { r#"
            # The crate

            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
            exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
            "#
        };

        let expected = indoc! { r#"
            This is a really nice crate.

            <!-- cargo-rdme start -->

            # The crate

            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
            exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

            <!-- cargo-rdme end -->

            Hope you enjoy!
            "#
        };

        let readme = Readme::from_str(readme_str);
        let doc = Doc::from_str(doc_str);

        let new_readme = inject_doc(&readme, &doc).unwrap();

        assert_eq!(new_readme.lines().join("\n"), expected.lines().join("\n"));
    }

    #[test]
    fn test_inject_doc_start_end_marker() {
        let readme_str = indoc! { r#"
            This is a really nice crate.

            <!-- cargo-rdme start -->

            Li Europan lingues es membres del sam familie. Lor separat existentie es un myth.
            Por scientie, musica, sport etc, litot Europa usa li sam vocabular.

            <!-- cargo-rdme end -->

            Hope you enjoy!
            "#
        };
        let doc_str = indoc! { r#"
            # The crate

            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
            exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
            "#
        };

        let expected = indoc! { r#"
            This is a really nice crate.

            <!-- cargo-rdme start -->

            # The crate

            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
            exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

            <!-- cargo-rdme end -->

            Hope you enjoy!
            "#
        };

        let readme = Readme::from_str(readme_str);
        let doc = Doc::from_str(doc_str);

        let new_readme = inject_doc(&readme, &doc).unwrap();

        assert_eq!(new_readme.lines().join("\n"), expected.lines().join("\n"));
    }
}
