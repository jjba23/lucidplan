/* lucidplan - free and open project management for everyone
 *
 * SPDX-License-Identifier: MIT
 * Copyright © Josep Bigorra <jjbigorra@gmail.com>
*/

// ==================================
// A planning table, kept current
// ==================================
//
// A table is the one page in LucidPlan that several people look at at the same
// time, each having done something the others cannot see yet: somebody has
// played a card, or the table has turned its cards over.  Nothing here decides
// what the page says.  The server renders the seats, the cards and the tally,
// and renders them again on the next request; this is only how a reader learns
// that asking again is worth it.
//
// The two things watched are watched differently, and the difference is the
// whole design.
//
// Whether each seat has played is cosmetic, so it is patched in place: the
// mark beside a name changes from one word to the other.  Both words travel to
// the browser as data on the seat's own element, because a sentence written
// into a script is a sentence no translator can reach -- the same reason
// `showQuietNotice' in `tickets.js' carries its wording as data.
//
// Whether the round has moved on, or the cards have been turned over, is not
// cosmetic: the page changes shape, from a hand of cards to a list of what
// everybody played, and the tally appears.  So the page is re-read rather than
// patched.  The server is the only thing that decides how a revealed round
// reads, and a second renderer here would be a second answer to the same
// question -- free to disagree with the first.

// Long enough not to waste a request per keystroke in a room of eight, short
// enough that the table does not feel like it is waiting on a person who has
// already played.  It is one small JSON answer per person per tick, and the
// request is answered from three indexed lookups.
const pokerPollInterval = 2500;

function pokerRoom() {
  return document.querySelector('[data-poker-room]');
}

// The mark beside a seat, found by the seat's own id rather than by its
// position: the seats are in the order people arrived, and one who joined
// while this page was open is a seat this page does not know about -- the
// reload that follows a round handles the arrival, so a mark that is missing
// is simply left alone.
function pokerSeatMark(seatId) {
  const seat = document.querySelector(`[data-poker-seat="${seatId}"]`);
  return seat ? seat.querySelector('[data-poker-mark]') : null;
}

async function readPokerState(room) {
  const url = room.getAttribute('data-poker-state-url');
  if (!url) return null;

  try {
    const response = await fetch(url, { headers: { Accept: 'application/json' } });
    if (!response.ok) return null;
    return await response.json();
  } catch (err) {
    // A meeting's wifi is not a reason to stop.  The next poll is a few
    // seconds away, and a page that gave up would be a page that quietly
    // stopped telling the truth.
    console.error('Failed to read the state of the table:', err);
    return null;
  }
}

// Whether the page is still about the round the table is on.  The room was
// drawn at one round with its cards one way up; either having changed means
// this page is describing something that is over.
function pokerRoundMoved(room, state) {
  const drawnRound = Number(room.getAttribute('data-poker-round') || '0');
  const drawnRevealed = room.getAttribute('data-poker-revealed') === 'true';
  return Number(state.round) !== drawnRound || Boolean(state.revealed) !== drawnRevealed;
}

function paintPokerSeats(state) {
  (state.seats || []).forEach((seat) => {
    const mark = pokerSeatMark(seat.id);
    if (!mark) return;
    const element = mark.closest('[data-poker-seat]');
    const word = seat.played
      ? element.getAttribute('data-poker-played')
      : element.getAttribute('data-poker-waiting');
    if (word && mark.textContent !== word) mark.textContent = word;
  });
}

// How many of the seats have played belongs to the same sentence the marks
// are: a panel whose heading still said one of two had played while both
// names below it said they had would be arguing with itself.  The number is
// written here rather than sent as wording, because it is only ever digits
// and a slash, and the words around it are the server's.
function paintPokerCount(room, state) {
  const count = room.querySelector('[data-poker-count]');
  if (!count) return;
  const seats = state.seats || [];
  const played = seats.filter((seat) => seat.played).length;
  const text = `(${played}/${seats.length})`;
  if (count.textContent !== text) count.textContent = text;
}

// Somebody midway through typing the next item should not have the words taken
// out of the field they are in, so a page that has to re-read itself waits for
// them to look away.  The change it was about to show is still there on the
// next tick.
function pokerTyping() {
  const active = document.activeElement;
  return Boolean(active && /^(INPUT|TEXTAREA|SELECT)$/.test(active.tagName));
}

async function pollPokerRoom(room) {
  const state = await readPokerState(room);
  if (!state) return;

  if (pokerRoundMoved(room, state)) {
    if (!pokerTyping()) window.location.reload();
    return;
  }

  paintPokerSeats(state);
  paintPokerCount(room, state);
}

document.addEventListener('DOMContentLoaded', () => {
  const room = pokerRoom();
  if (!room) return;

  // A page nobody is looking at has nothing to keep current: a table left open
  // in a tab overnight should not poll until morning.  Coming back to the tab
  // asks once and starts again, so the reader is never shown a stale table.
  let timer = null;

  const tick = () => pollPokerRoom(room);
  const start = () => {
    if (!timer) timer = window.setInterval(tick, pokerPollInterval);
  };
  const stop = () => {
    if (timer) {
      window.clearInterval(timer);
      timer = null;
    }
  };

  document.addEventListener('visibilitychange', () => {
    if (document.hidden) {
      stop();
    } else {
      tick();
      start();
    }
  });

  if (!document.hidden) {
    tick();
    start();
  }
});
