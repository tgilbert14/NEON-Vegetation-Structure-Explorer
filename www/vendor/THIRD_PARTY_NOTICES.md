# Third-party browser assets

These browser dependencies are vendored so the app's essential interface and exports do not depend on a public CDN at runtime.

| Package | Version | Upstream | License | Vendored files |
|---|---:|---|---|---|
| SweetAlert2 | 11.10.0 | <https://github.com/sweetalert2/sweetalert2> | MIT | `sweetalert2/sweetalert2.all.min.js`, `sweetalert2/sweetalert2.min.css` |
| html-to-image | 1.11.13 | <https://github.com/bubkoo/html-to-image> | MIT | `html-to-image/html-to-image.js` |
| Driver.js | 1.3.1 | <https://github.com/kamranahmedse/driver.js> | MIT | `driver/driver.js.iife.js`, `driver/driver.css` |

The complete license text distributed by each project is retained beside its files as `LICENSE`.

Vendored asset bytes are release inputs and must be included in the Connect manifest and runtime receipt. Updating a version requires updating this notice, the adjacent license, the manifest, and the release checksums together.
