/* lucidplan - free and open project management for everyone
 *
 * Copyright © Josep Bigorra <jjbigorra@gmail.com>
 *
 * lucidplan is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * lucidplan is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with lucidplan.  If not, see <https://www.gnu.org/licenses/>.
 */

/* A password field that says how strong the password being typed is.
 *
 * The estimate is the one the server enforces: it mirrors bewaking's own
 * `password-strength`, so the meter a reader watches fill is measuring the
 * same thing the server will measure when the form is submitted -- a bar
 * that reaches "strong" is a form the server will accept.  Both are only an
 * estimate of guessing difficulty; neither is a guarantee.
 *
 * The control is progressive: a password field is a password field with the
 * script disabled, and the meter is only ever added beside one that carries
 * the `data-password-strength' marker.  Every word the meter shows travels
 * in that marker's data attributes, translated by the server, so no sentence
 * is written into this file where it could not be.
 */

// @ts-check
"use strict";

(function () {
    // The very common passwords bewaking caps at zero, kept in step with it.
    var COMMON_PASSWORDS = [
        "123456", "password", "12345678", "qwerty", "123456789", "12345",
        "1234", "111111", "1234567", "dragon", "123123", "baseball", "abc123",
        "football", "monkey", "letmein", "shadow", "master", "666666",
        "qwertyuiop", "123321", "mustang", "1234567890", "michael", "654321",
        "superman", "1qaz2wsx", "7777777", "121212", "000000", "iloveyou",
        "admin", "welcome", "login", "princess", "solo", "starwars",
        "passw0rd", "hello", "freedom", "whatever"
    ];

    // Roughly how many characters each class draws from, as bewaking counts
    // them: the size of the search space the estimate is built on.
    var POOL_SIZES = { lowercase: 26, uppercase: 26, digit: 10, symbol: 33 };

    // The colour the bar wears at each score, weakest to strongest.  They are
    // set inline rather than as utility classes, because the stylesheet is
    // precompiled and a class this file invented might not be in it.
    var COLORS = ["#b91c1c", "#b91c1c", "#b45309", "#15803d", "#15803d"];

    function log2(x) {
        return Math.log(x) / Math.log(2);
    }

    function characterClasses(password) {
        var classes = [];
        if (/[a-z]/.test(password)) classes.push("lowercase");
        if (/[A-Z]/.test(password)) classes.push("uppercase");
        if (/[0-9]/.test(password)) classes.push("digit");
        if (/[^A-Za-z0-9\s]/.test(password)) classes.push("symbol");
        return classes;
    }

    function poolSize(classes) {
        return classes.reduce(function (total, name) {
            return total + (POOL_SIZES[name] || 0);
        }, 0);
    }

    function longestRun(password, same) {
        if (password.length < 2) return password.length;
        var longest = 1;
        var current = 1;
        for (var i = 1; i < password.length; i++) {
            if (same(password[i - 1], password[i])) {
                current += 1;
                if (current > longest) longest = current;
            } else {
                current = 1;
            }
        }
        return longest;
    }

    function sequentialRun(password) {
        if (password.length < 2) return password.length;
        var longest = 1;
        var current = 1;
        for (var i = 1; i < password.length; i++) {
            var delta = password.charCodeAt(i) - password.charCodeAt(i - 1);
            if (delta === 1 || delta === -1) {
                current += 1;
                if (current > longest) longest = current;
            } else {
                current = 1;
            }
        }
        return longest;
    }

    function strengthScore(password) {
        if (password.length < 1) return 0;

        var common = COMMON_PASSWORDS.indexOf(password.toLowerCase()) !== -1;
        var raw = password.length * log2(poolSize(characterClasses(password)));
        var repeated = longestRun(password, function (a, b) {
            return a === b;
        });
        var sequential = sequentialRun(password);
        var penalty = (common ? raw : 0)
            + 4 * Math.max(0, repeated - 2)
            + 4 * Math.max(0, sequential - 2)
            + (password.length < 8 ? 6 : 0);
        var entropy = Math.max(0, raw - penalty);

        if (common) return 0;
        if (entropy < 28) return 0;
        if (entropy < 40) return 1;
        if (entropy < 60) return 2;
        if (entropy < 80) return 3;
        return 4;
    }

    // The label is read from the attribute itself rather than through
    // `dataset': a dataset property name only uppercases the letter that
    // follows a hyphen, so `data-strength-label-0' is the property
    // `strengthLabel-0' -- not `strengthLabel0' -- and the camelCase lookup
    // this used to do found nothing to show.
    function labelFor(container, score) {
        var label = container.getAttribute("data-strength-label-" + score);
        return label === null ? "" : label;
    }

    function attach(container) {
        var input = container.querySelector("input[type=password]")
            || container.querySelector("input");
        if (!input) return;

        var minScore = parseInt(container.dataset.minScore || "3", 10);

        var meter = document.createElement("div");
        meter.style.marginTop = "0.25rem";

        var track = document.createElement("div");
        track.style.height = "0.5rem";
        track.style.width = "100%";
        track.style.maxWidth = "28rem";
        track.style.borderRadius = "9999px";
        track.style.overflow = "hidden";
        track.style.background = "rgba(107, 114, 128, 0.3)";

        var fill = document.createElement("div");
        fill.style.height = "100%";
        fill.style.width = "0%";
        fill.style.transition = "width 120ms ease-out, background-color 120ms ease-out";
        track.appendChild(fill);

        var label = document.createElement("p");
        label.style.fontSize = "0.875rem";
        label.style.marginTop = "0.25rem";
        label.setAttribute("aria-live", "polite");

        var hint = document.createElement("p");
        hint.style.fontSize = "0.75rem";
        hint.style.opacity = "0.8";
        if (container.dataset.strengthHint) {
            hint.textContent = container.dataset.strengthHint;
        }

        meter.appendChild(track);
        meter.appendChild(label);
        if (hint.textContent) {
            meter.appendChild(hint);
        }

        // Beside the field, not over it: the meter is a reading of what is
        // being typed, so it belongs where the eye already is.
        if (input.parentNode) {
            input.parentNode.insertBefore(meter, input.nextSibling);
        } else {
            container.appendChild(meter);
        }

        function update() {
            var score = strengthScore(input.value);
            var meets = score >= minScore;
            fill.style.width = (score === 0 && input.value.length === 0)
                ? "0%"
                : ((score + 1) * 20) + "%";
            fill.style.background = COLORS[score];
            label.textContent = input.value.length === 0
                ? ""
                : labelFor(container, score) + (meets ? " ✓" : "");
        }

        input.addEventListener("input", update);
        update();
    }

    function init() {
        var fields = document.querySelectorAll("[data-password-strength]");
        for (var i = 0; i < fields.length; i++) {
            attach(fields[i]);
        }
    }

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", init);
    } else {
        init();
    }
})();
