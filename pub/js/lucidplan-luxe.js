/* lucidplan - free and open project management for everyone
 *
 * SPDX-License-Identifier: MIT
 * Copyright © Josep Bigorra <jjbigorra@gmail.com>
 */

// The page's polish, in one script: the light that follows the pointer, the
// lean of a card under it, the reveal of a section as it is scrolled into
// view, and the deepening of the toolbar once the page has moved under it.
//
// None of it is required.  Every one of these effects is stated in
// `lucidplan-luxe.css' with a resting state that is already the right answer
// -- a card that is not tilted is a card at zero degrees, a section that is
// not revealed is a section in plain view once the `lp-js' class says a
// script is running at all -- so a reader whose script did not load, or who
// asked their system for less motion, reads the same page with less of it
// moving.  Nothing here hides or reveals content, and nothing here knows what
// any of the content says.

(function () {
  'use strict';

  const reduced = window.matchMedia('(prefers-reduced-motion: reduce)');

  // ===== The light that follows the pointer =====

  // One listener on the document rather than one per card: a page can hold
  // dozens of lit surfaces and a listener apiece would be dozens of closures
  // doing the same arithmetic.  The pointer's position is kept until the next
  // frame and then painted once, because a pointer moves several times per
  // frame and writing two custom properties per move would lay out the card
  // more often than the screen can show it.
  const TILT_LIMIT = 3.5;

  let frame = null;
  let pending = null;

  function paint() {
    frame = null;

    if (!pending) {
      return;
    }

    const lit = pending.lit;
    const box = lit.getBoundingClientRect();

    if (!box.width || !box.height) {
      pending = null;
      return;
    }

    // Where the pointer is, said as a percentage of the surface, which is
    // what the radial gradient in the stylesheet reads: a percentage follows
    // the card when it is resized or scrolled, an offset would not.
    const x = ((pending.x - box.left) / box.width) * 100;
    const y = ((pending.y - box.top) / box.height) * 100;

    lit.style.setProperty('--lp-x', x.toFixed(2) + '%');
    lit.style.setProperty('--lp-y', y.toFixed(2) + '%');

    // The lean is the pointer's distance from the middle of the card, at most
    // a few degrees: enough for the surface to feel like a plane catching the
    // light, far too little for any text on it to look crooked.  A card that
    // is only being lit (`lp-spotlight' alone) is left flat.
    if (lit.classList.contains('lp-tilt')) {
      const rx = (0.5 - y / 100) * 2 * TILT_LIMIT;
      const ry = (x / 100 - 0.5) * 2 * TILT_LIMIT;
      lit.style.setProperty('--lp-rx', rx.toFixed(2) + 'deg');
      lit.style.setProperty('--lp-ry', ry.toFixed(2) + 'deg');
    }

    pending = null;
  }

  function litSurfaceUnder(node) {
    return node instanceof Element
      ? node.closest('.lp-spotlight, .lp-tilt')
      : null;
  }

  document.addEventListener(
    'pointermove',
    function (event) {
      if (reduced.matches) {
        return;
      }

      // A finger or a pen does not hover, and following one would leave the
      // last card touched lit and leaning after the finger has gone.
      if (event.pointerType && event.pointerType !== 'mouse') {
        return;
      }

      const lit = litSurfaceUnder(event.target);

      if (!lit) {
        return;
      }

      pending = { lit: lit, x: event.clientX, y: event.clientY };

      if (frame === null) {
        frame = window.requestAnimationFrame(paint);
      }
    },
    { passive: true }
  );

  // Leaving a card puts it back the way it was.  `relatedTarget' is where the
  // pointer went; a move from the card onto its own padding still reports the
  // card itself as the one being left, and that is not a leave.
  document.addEventListener(
    'pointerout',
    function (event) {
      const tilted =
        event.target instanceof Element ? event.target.closest('.lp-tilt') : null;

      if (!tilted || tilted.contains(event.relatedTarget)) {
        return;
      }

      tilted.style.removeProperty('--lp-rx');
      tilted.style.removeProperty('--lp-ry');
    },
    { passive: true }
  );

  // ===== Sections that arrive =====

  // A section marked `lp-rise' is hidden by the stylesheet only while the
  // document carries `lp-js', which the page's head adds before anything is
  // painted.  The reveal is the script taking that back off, one section at a
  // time, as each is scrolled near enough to be read.
  //
  // This runs once the document has been read rather than when the script is
  // reached, because the script is written into the page's head: at that
  // moment there is no section to reveal yet, and a query that ran then would
  // find nothing and quietly leave the page with nothing to reveal.
  function ready(run) {
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', run, { once: true });
    } else {
      run();
    }
  }

  ready(function () {
    const risers = Array.prototype.slice.call(
      document.querySelectorAll('.lp-rise')
    );

    function reveal(riser) {
      riser.classList.add('lp-revealed');
    }

    function revealAll() {
      risers.forEach(reveal);
    }

    if (risers.length) {
      if (reduced.matches || !('IntersectionObserver' in window)) {
        // Nothing is animated, so everything is simply shown: a reader who
        // asked for less motion should not have to scroll to make a section
        // appear.
        revealAll();
      } else {
        const watcher = new IntersectionObserver(
          function (entries) {
            entries.forEach(function (entry) {
              if (!entry.isIntersecting) {
                return;
              }

              reveal(entry.target);
              watcher.unobserve(entry.target);
            });
          },
          // The bottom margin is negative so a section is revealed when it has
          // genuinely come into view rather than while it is still a sliver at
          // the foot of the screen.
          { rootMargin: '0px 0px -8% 0px', threshold: 0.08 }
        );

        risers.forEach(function (riser) {
          watcher.observe(riser);
        });
      }
    }

    // The preference can be changed while the page is open, and a reader who
    // turns motion off then should not be left with sections that never
    // reveal because they were waited for rather than shown.
    if (typeof reduced.addEventListener === 'function') {
      reduced.addEventListener('change', function () {
        if (reduced.matches) {
          revealAll();
        }
      });
    }

    // ===== Items that unfold =====

    // A backlog is a list of cards a reader opens one at a time.  The markup
    // carries the whole of each card already -- the row, and beneath it the
    // body it opens into -- so all that is missing without a script is the
    // fold itself, and that is what this adds: the rows are decorated into
    // controls here rather than in the HTML, so a reader whose script never
    // ran is not handed a button that does nothing, and reads every card open
    // instead.
    //
    // The opening and closing are the stylesheet's: this only says which item
    // is open, by the one class the reveal is written against.  Nothing is
    // measured and nothing is set inline, which is what lets a body change
    // size while it is open -- a ticket's status menu, a form that grew an
    // error line -- and stay right.
    const folds = Array.prototype.slice.call(
      document.querySelectorAll('.lp-disclosure')
    );

    function setFold(item, open) {
      item.classList.toggle('lp-disclosure-open', open);

      const row = item.querySelector('.lp-disclosure-summary');

      if (row && row.hasAttribute('aria-expanded')) {
        row.setAttribute('aria-expanded', open ? 'true' : 'false');
      }
    }

    if (folds.length) {
      folds.forEach(function (item) {
        const row = item.querySelector('.lp-disclosure-summary');
        const body = item.querySelector('.lp-disclosure-body');

        if (!row || !body) {
          return;
        }

        row.setAttribute('role', 'button');
        row.setAttribute('tabindex', '0');
        row.setAttribute('aria-expanded', 'false');

        if (body.id) {
          row.setAttribute('aria-controls', body.id);
        }
      });

      // The controls a row may hold keep their own job: the link to the
      // ticket, the grip that drags the row, a menu that posts a change.  Only
      // a click on the row itself -- its ink, its padding -- opens the card.
      const OWN_CONTROL = 'a, button, input, select, textarea, label, [draggable="true"]';

      document.addEventListener('click', function (event) {
        const node = event.target instanceof Element ? event.target : null;

        if (!node) {
          return;
        }

        // Two buttons that open and close the whole list at once.
        if (node.closest('[data-lp-expand-all]')) {
          folds.forEach(function (item) {
            setFold(item, true);
          });
          return;
        }

        if (node.closest('[data-lp-collapse-all]')) {
          folds.forEach(function (item) {
            setFold(item, false);
          });
          return;
        }

        const row = node.closest('.lp-disclosure-summary');

        if (!row) {
          return;
        }

        const own = node.closest(OWN_CONTROL);

        if (own && row.contains(own)) {
          return;
        }

        const item = row.closest('.lp-disclosure');

        if (item) {
          setFold(item, !item.classList.contains('lp-disclosure-open'));
        }
      });

      // A row is a control to a keyboard too: it is focusable and it opens on
      // Enter or Space, the two keys a button answers.  A control inside it
      // answers its own.
      document.addEventListener('keydown', function (event) {
        if (event.key !== 'Enter' && event.key !== ' ' && event.key !== 'Spacebar') {
          return;
        }

        const node = event.target instanceof Element ? event.target : null;

        if (!node || !node.classList || !node.classList.contains('lp-disclosure-summary')) {
          return;
        }

        event.preventDefault();

        const item = node.closest('.lp-disclosure');

        if (item) {
          setFold(item, !item.classList.contains('lp-disclosure-open'));
        }
      });
    }

    // ===== A toolbar that knows the page moved =====

    // A desktop toolbar draws a shadow once there is content under it, and
    // that shadow is what tells a reader the bar is above the page rather than
    // part of it.  The bar is `lp-toolbar'; the class toggled here is the one
    // the stylesheet deepens.
    const toolbar = document.querySelector('.lp-toolbar');

    if (toolbar) {
      let ticking = false;

      function settleToolbar() {
        ticking = false;
        toolbar.classList.toggle('lp-scrolled', window.scrollY > 8);
      }

      window.addEventListener(
        'scroll',
        function () {
          if (ticking) {
            return;
          }

          ticking = true;
          window.requestAnimationFrame(settleToolbar);
        },
        { passive: true }
      );

      // A page is rarely opened at its own top -- a reload keeps the scroll
      // position -- so the bar is told where the page is before the reader
      // touches it.
      settleToolbar();
    }
  });
})();
