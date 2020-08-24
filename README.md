# classify

See [a question on Stack Overflow][q] for motivation. This repository contains several ways to split a list into equivalence classes, and a benchmark to compare their performance. Each lives in its own module. List of modules with links to their place of origin:

* [`Fischer`](https://stackoverflow.com/a/8262250)
* [`Classify`](https://stackoverflow.com/a/57761458)
* [`Projection`](https://stackoverflow.com/a/8262250) _(My own interpretation, so all errors are mine.)_

[q]: https://stackoverflow.com/q/8262179

Run like this:

    % cabal run classify-benchmark -- --output report.html

— Then see the file `report.html` thus generated for nice pictures. The [`report.html`](report.html) added to the repository shows what I see.
