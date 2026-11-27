/* lucidplan - free and open project management for everyone
 *
 * SPDX-License-Identifier: MIT
 * Copyright © Josep Bigorra <jjbigorra@gmail.com>
 */

// A `<select>` that opens a menu of the page's own
//
// Every menu in the application is a `<select>`, and it should be: the value,
// the change event, form submission, and a screen reader's idea of a list of
// choices are all the native control's already.  What the native control
// cannot be told is what its *popup* looks like -- the list the platform
// draws, on a dark page, is a dark face under the ink the page chose for a
// light one, which is how the entries came to be illegible.
//
// So the `<select>` stays and is hidden, and this script draws a control and a
// menu for it in the page's own colours: an `<lp-select>` element holding a
// `role="combobox"` button and a `role="listbox"` of `role="option"` rows.
// Everything a reader does lands on the select -- choosing a row sets its
// value and fires its `change`, which is what the page's own handler is
// listening for -- so no view has to know this exists.
//
// The drawn control goes where the select was, in the select's own parent, so
// the page keeps the layout it wrote: the element is a box of the control's
// size, and the menu hangs from that box rather than from the field around it.
//
// A reader whose script did not load meets the native `<select>` and nothing
// else: the select is hidden by a class this script adds, and no page is
// changed by markup.
//
// Nothing here knows what any row says: a row's words are the option's own
// text and its value is the option's own value.

(function () {
  'use strict';

  const SVG_NS = 'http://www.w3.org/2000/svg';
  const CHEVRON = 'M4.2 6.4 8 10.2l3.8-3.8';

  // The next letter typed within this long of the last one continues the
  // search; after it, a new one starts.  Roughly the window a native select
  // uses.
  const TYPEAHEAD_IDLE = 600;

  // How much room the menu wants under (or above) the control before the menu
  // is turned the other way.  A menu that opens off the screen is a menu a
  // reader cannot reach, so the side with room wins even when it is the top.
  const ROOM = 180;

  let serial = 0;

  // The element the control and the menu are drawn in.  It holds no state of
  // its own -- the state is the select's, and the closures below keep the rest
  // -- so it is defined for the stylesheet to name and for the page to see as
  // a real element rather than an unknown tag.
  class LpSelect extends HTMLElement {}

  if (!customElements.get('lp-select')) {
    customElements.define('lp-select', LpSelect);
  }

  function upgrade(select) {
    // Once per select: the class is both what hides the native control and
    // what says this one has already been drawn.
    if (select.classList.contains('lp-select-native')) {
      return;
    }

    // The width the control had, taken while it still had it: a select is as
    // wide as its widest row, and a menu drawn only as wide as the value it
    // shows would move the field it sits in every time a reader chose.
    const width = select.getBoundingClientRect().width;

    const id = select.id || 'lp-select-' + (++serial);
    const listId = id + '-list';

    // The name the control is announced by: what the page already called the
    // select, either its own `aria-label` or the `<label>` tied to it by
    // `for`.  The words on the button are the chosen value, so a name taken
    // from them would change with the choice and never say what the control is
    // for.
    const label = select.getAttribute('aria-label');
    const tied =
      !label && select.id
        ? document.querySelector('label[for="' + CSS.escape(select.id) + '"]')
        : null;
    const name = label || (tied && tied.textContent.trim());

    const shell = document.createElement('lp-select');

    const control = document.createElement('button');
    control.type = 'button';
    // The page's own classes for this control come along, so what it looks
    // like stays the decision of the view that wrote it.
    control.className =
      'lp-select-control' + (select.className ? ' ' + select.className : '');
    control.setAttribute('role', 'combobox');
    control.setAttribute('aria-haspopup', 'listbox');
    control.setAttribute('aria-expanded', 'false');
    control.setAttribute('aria-controls', listId);

    if (name) {
      control.setAttribute('aria-label', name);
    }

    // What the select says about itself beyond its name is what the control
    // says too: the field's description, and whether the field is required.
    const described = select.getAttribute('aria-describedby');
    if (described) {
      control.setAttribute('aria-describedby', described);
    }

    if (select.required) {
      control.setAttribute('aria-required', 'true');
    }

    if (select.disabled) {
      control.disabled = true;
    }

    if (width) {
      control.style.minWidth = Math.ceil(width) + 'px';
    }

    // A label is a target too: the platform would have forwarded a click on it
    // to the select, which is the control this script hid.
    if (tied) {
      tied.addEventListener('click', (event) => {
        event.preventDefault();
        control.focus();
      });
    }

    const value = document.createElement('span');
    value.className = 'lp-select-value';

    const chevron = document.createElementNS(SVG_NS, 'svg');
    chevron.setAttribute('class', 'lp-select-chevron');
    chevron.setAttribute('viewBox', '0 0 16 16');
    chevron.setAttribute('aria-hidden', 'true');

    const stroke = document.createElementNS(SVG_NS, 'path');
    stroke.setAttribute('d', CHEVRON);
    stroke.setAttribute('fill', 'none');
    stroke.setAttribute('stroke', 'currentColor');
    stroke.setAttribute('stroke-width', '1.7');
    stroke.setAttribute('stroke-linecap', 'round');
    stroke.setAttribute('stroke-linejoin', 'round');
    chevron.appendChild(stroke);

    control.append(value, chevron);

    const list = document.createElement('ul');
    list.className = 'lp-select-list';
    list.id = listId;
    list.setAttribute('role', 'listbox');
    list.tabIndex = -1;
    list.hidden = true;

    if (name) {
      list.setAttribute('aria-label', name);
    }

    // One row per option, read once.  A select's options are part of the page
    // it was rendered with.
    // shortcut: this answers a select that picks one thing, and reads its
    // options once -- a multiple select or one whose options change in place
    // wants its own element.
    const rows = Array.prototype.map.call(select.options, (option, index) => {
      const row = document.createElement('li');
      row.className = 'lp-select-option';
      row.id = listId + '-row-' + index;
      row.setAttribute('role', 'option');
      row.setAttribute('data-value', option.value);
      row.textContent = option.text;
      if (option.disabled) {
        row.setAttribute('aria-disabled', 'true');
      }

      list.appendChild(row);
      return row;
    });

    shell.append(control, list);
    select.after(shell);
    select.classList.add('lp-select-native');

    let open = false;
    let active = 0;
    let typed = '';
    let typedAt = 0;

    const paint = () => {
      const chosen = select.options[select.selectedIndex];
      value.textContent = chosen ? chosen.text : '';
      rows.forEach((row, index) => {
        row.setAttribute(
          'aria-selected',
          index === select.selectedIndex ? 'true' : 'false'
        );
      });

      // A control that was refused because nothing was chosen stops saying so
      // as soon as something is.  `validity' is read rather than
      // `checkValidity()' so that reading it does not fire `invalid' again.
      if (control.hasAttribute('aria-invalid') && select.validity.valid) {
        control.removeAttribute('aria-invalid');
        shell.classList.remove('lp-select-needed');
      }
    };

    const activate = (index) => {
      active = Math.max(0, Math.min(index, rows.length - 1));
      rows.forEach((row, i) => {
        const here = i === active;
        row.classList.toggle('lp-select-active', here);

        if (here) {
          control.setAttribute('aria-activedescendant', row.id);
          // The menu scrolls, not the page: `nearest` moves the least it can
          // and only when the row is out of view.
          row.scrollIntoView({ block: 'nearest' });
        }
      });
    };

    // The menu hangs from the control, on the side with room for it: under the
    // control unless the control is near the foot of the screen, and to the
    // control's own start unless the menu would reach past the far edge.
    const place = () => {
      shell.classList.remove('lp-select-up', 'lp-select-end');

      const box = control.getBoundingClientRect();
      const under = window.innerHeight - box.bottom;

      if (under < ROOM && box.top > under) {
        shell.classList.add('lp-select-up');
      }

      if (box.left + list.offsetWidth > window.innerWidth - 8) {
        shell.classList.add('lp-select-end');
      }
    };

    const expand = () => {
      if (open || !rows.length || select.disabled) {
        return;
      }

      open = true;
      list.hidden = false;
      shell.classList.add('lp-select-open');
      control.setAttribute('aria-expanded', 'true');
      place();
      activate(Math.max(0, select.selectedIndex));
    };

    const collapse = (refocus) => {
      if (!open) {
        return;
      }

      open = false;
      list.hidden = true;
      shell.classList.remove('lp-select-open');
      control.setAttribute('aria-expanded', 'false');
      control.removeAttribute('aria-activedescendant');

      if (refocus) {
        control.focus();
      }
    };

    const choose = (index) => {
      if (!select.options[index] || select.options[index].disabled) {
        return;
      }

      select.selectedIndex = index;
      // The page's own handler is an `onchange` on the select, so this is the
      // whole of what "a row was chosen" has ever meant here.
      select.dispatchEvent(new Event('change', { bubbles: true }));
      paint();
      collapse(true);
    };

    // Letters, in the order they were typed, find the next row that starts
    // with them -- the way a native select answers.
    const typeahead = (key) => {
      const now = Date.now();
      typed = now - typedAt > TYPEAHEAD_IDLE ? key : typed + key;
      typedAt = now;

      const needle = typed.toLowerCase();
      const from = (open ? active : select.selectedIndex) + 1;

      for (let step = 0; step < rows.length; step += 1) {
        const index = (from + step + rows.length) % rows.length;

        if (
          !select.options[index].disabled &&
          rows[index].textContent.trim().toLowerCase().startsWith(needle)
        ) {
          if (open) {
            activate(index);
          } else {
            choose(index);
          }

          return;
        }
      }
    };

    control.addEventListener('click', () => {
      if (open) {
        collapse(false);
      } else {
        expand();
      }
    });

    // Clicking a row must not take the focus off the button: the row is the
    // menu's target and the button is where the keyboard lives, and a focus
    // that leaves the element collapses the menu before the click lands.
    list.addEventListener('pointerdown', (event) => event.preventDefault());

    list.addEventListener('click', (event) => {
      const node = event.target instanceof Element ? event.target : null;
      const row = node && node.closest('.lp-select-option');

      if (row) {
        choose(rows.indexOf(row));
      }
    });

    control.addEventListener('keydown', (event) => {
      if (event.altKey || event.ctrlKey || event.metaKey) {
        return;
      }

      switch (event.key) {
        case 'ArrowDown':
        case 'Down':
        case 'ArrowUp':
        case 'Up':
          event.preventDefault();

          if (open) {
            activate(active + (event.key === 'ArrowUp' || event.key === 'Up' ? -1 : 1));
          } else {
            expand();
          }

          break;

        case 'Home':
        case 'End':
          if (!open) {
            return;
          }

          event.preventDefault();
          activate(event.key === 'Home' ? 0 : rows.length - 1);
          break;

        case 'Enter':
        case ' ':
        case 'Spacebar':
          event.preventDefault();

          if (open) {
            choose(active);
          } else {
            expand();
          }

          break;

        case 'Escape':
        case 'Esc':
          if (!open) {
            return;
          }

          event.preventDefault();
          collapse(true);
          break;

        case 'Tab':
          collapse(false);
          break;

        default:
          if (event.key.length === 1) {
            typeahead(event.key);
          }
      }
    });

    const outside = (event) => {
      if (open && !shell.contains(event.target)) {
        collapse(false);
      }
    };

    document.addEventListener('pointerdown', outside);

    // A focus that leaves the widget folds it -- but a pointer that moves from
    // the button into the menu has not left it.
    shell.addEventListener('focusout', (event) => {
      if (!shell.contains(event.relatedTarget)) {
        collapse(false);
      }
    });

    ['change', 'input'].forEach((type) => select.addEventListener(type, paint));

    // A menu that a form requires is the one thing the platform cannot refuse
    // out loud: the control it would focus to say so is the one this script
    // hid, so pressing the form's own button would do nothing at all.  The
    // refusal is answered where the platform says it -- the `invalid' event,
    // which fires before any submit does -- and answered the same way: the
    // control says it is not filled in, and the menu opens on the row still to
    // be chosen.
    //
    // The event does not bubble, so it is heard here; `validity' is read rather
    // than `checkValidity()' because the latter would fire the event again.
    if (select.willValidate) {
      select.addEventListener('invalid', () => {
        // The first field that is not filled in is the one a reader is sent to,
        // and it is not this menu's job to take another menu's turn.
        const first = Array.prototype.find.call(
          select.form ? select.form.elements : [select],
          (field) => field.willValidate && !field.validity.valid
        );

        if (first !== select) {
          return;
        }

        control.setAttribute('aria-invalid', 'true');
        shell.classList.add('lp-select-needed');
        control.focus();
        expand();
      });
    }

    // A browser puts a select's remembered value back after the page has been
    // read, without an event the control could hear.
    window.addEventListener('load', paint, { once: true });

    paint();
  }

  // Every menu on the page is drawn here, because the platform's list is the
  // one thing a page cannot colour, and a page that drew some of its menus and
  // left the rest to the platform would be two interfaces.
  function upgradeAll() {
    Array.prototype.forEach.call(document.querySelectorAll('select'), upgrade);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', upgradeAll, { once: true });
  } else {
    upgradeAll();
  }
})();
