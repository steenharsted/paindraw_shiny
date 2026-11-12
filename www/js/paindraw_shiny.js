/*
 * paindraw_shiny.js -- the drawing canvas.
 *
 * Ported from PainDrawEM js/paindraw.js, so the recorded strokes match what
 * the REDCap module records.
 *
 * How this file talks to R (Shiny):
 *   R -> JS  session$sendCustomMessage("pd-...", list(...)) calls the
 *            handlers registered with Shiny.addCustomMessageHandler() below.
 *   JS -> R  Shiny.setInputValue("<canvas id>_state", {...}) shows up in R
 *            as input$canvas_state inside the canvas module.
 * The drawing itself happens only in the browser; R is not involved per point.
 */
(function () {
  'use strict';

  // ---- Seeded random numbers --------------------------------------------
  // Like set.seed() in R: each stroke gets its own seed, so a spray stroke
  // can be redrawn dot for dot after an undo. The seed is not saved in the JSON.
  function seededRandom(seed) {
    let a = seed >>> 0;
    return function () { // mulberry32
      a = (a + 0x6D2B79F5) >>> 0;
      let t = a;
      t = Math.imul(t ^ (t >>> 15), t | 1);
      t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
      return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
  }

  // ---- One "pad" per canvas, kept in a lookup table by canvas id ----------
  const pads = {};

  function getPad(id) {
    if (!pads[id]) {
      const canvas = document.getElementById(id);
      if (!canvas) return null;
      pads[id] = makePad(canvas);
    }
    return pads[id];
  }

  function makePad(canvas) {
    const pad = {
      canvas: canvas,
      ctx: canvas.getContext('2d'),
      // Tool settings, replaced by the server ("pd-settings")
      settings: { tool: 'spray', color: '#fc3d03', lineWidth: 3,
                  sprayDensity: 15, sprayRadius: 5, alpha: 0.1 },
      background: null, // the loaded background Image; null = can't draw yet
      loadCount: 0,     // to ignore background images that finish loading too late
      token: null,      // echoed back to R, see mod_canvas.R
      strokes: [],      // finished strokes
      current: null,    // the stroke being drawn, or null
      pointerId: null,  // the pointer (mouse/finger/stylus) drawing it
      under: null       // pen only: canvas pixels from before the current stroke
    };
    canvas.addEventListener('pointerdown', function (e) { onDown(pad, e); });
    canvas.addEventListener('pointermove', function (e) { onMove(pad, e); });
    canvas.addEventListener('pointerup', function (e) { onUp(pad, e); });
    canvas.addEventListener('pointercancel', function (e) { onUp(pad, e); });
    canvas.addEventListener('contextmenu', function (e) { e.preventDefault(); });
    return pad;
  }

  // ---- Pointer position -> canvas pixels ----------------------------------
  // The canvas is shown scaled (CSS max-width), so divide screen pixels by the
  // display scale, as PainDrawEM does with its scaleFactor.
  function canvasPoint(pad, e) {
    const rect = pad.canvas.getBoundingClientRect();
    const sx = rect.width / pad.canvas.width;
    const sy = rect.height / pad.canvas.height;
    return {
      x: Math.round((e.clientX - rect.left) / sx),
      y: Math.round((e.clientY - rect.top) / sy),
      scaleFactor: sx
    };
  }

  function inside(pad, pt) {
    return pt.x >= 0 && pt.y >= 0 && pt.x < pad.canvas.width && pt.y < pad.canvas.height;
  }

  // ---- Pointer events (mouse, touch and stylus alike) ---------------------
  function onDown(pad, e) {
    if (!pad.background || pad.current || !e.isPrimary) return; // one pointer at a time
    if (e.pointerType === 'mouse' && e.button !== 0) return;     // left button only
    const pt = canvasPoint(pad, e);
    if (!inside(pad, pt)) return;
    e.preventDefault();
    pad.canvas.setPointerCapture(e.pointerId); // keep receiving moves outside the canvas
    pad.pointerId = e.pointerId;

    // Settings are captured per stroke, at stroke start (as PainDrawEM)
    const s = pad.settings;
    const spray = s.tool === 'spray';
    pad.current = {
      strokeId: pad.strokes.length + 1,
      // A stylus counts as touch, like PainDrawEM's touch emulation
      drawInputType: e.pointerType === 'mouse' ? 'mouse' : 'touch',
      tool: s.tool,
      color: s.color,
      lineWidth: s.lineWidth,
      sprayDensity: spray ? s.sprayDensity : null,
      sprayRadius: spray ? s.sprayRadius : null,
      alpha: s.alpha,
      scaleFactor: pt.scaleFactor,
      dpr: window.devicePixelRatio || 1,
      p: [],
      seed: Math.floor(Math.random() * 4294967296)
    };
    pad.current.random = seededRandom(pad.current.seed);
    // Pen: remember the canvas as it is before this stroke (see addPoint)
    if (pad.current.tool === 'pen') {
      pad.under = pad.ctx.getImageData(0, 0, pad.canvas.width, pad.canvas.height);
    }
    addPoint(pad, pt.x, pt.y);
  }

  function onMove(pad, e) {
    if (!pad.current || e.pointerId !== pad.pointerId) return;
    const pt = canvasPoint(pad, e);
    if (inside(pad, pt)) addPoint(pad, pt.x, pt.y); // points outside are dropped
  }

  function onUp(pad, e) {
    if (!pad.current || e.pointerId !== pad.pointerId) return;
    pad.strokes.push(pad.current);
    pad.current = null;
    pad.pointerId = null;
    pad.under = null;
    report(pad);
  }

  // ---- Drawing (doSpray / doPen in PainDrawEM) ----------------------------
  function addPoint(pad, x, y) {
    const stroke = pad.current;
    stroke.p.push([x, y]);
    if (stroke.tool === 'spray') {
      drawSpray(pad.ctx, stroke, x, y); // spray: add this point's dots
    } else {
      // Pen: put back the canvas from before the stroke, then draw the whole
      // stroke once. Unlike PainDrawEM, which strokes the growing path again
      // for every point, so a semi-transparent pen line darkens to opaque.
      pad.ctx.putImageData(pad.under, 0, 0);
      drawPen(pad.ctx, stroke);
    }
  }

  // sprayDensity squares of side lineWidth, randomly within sprayRadius.
  // Every square is painted on its own, so overlapping dots add up.
  function drawSpray(ctx, stroke, x, y) {
    ctx.globalAlpha = stroke.alpha;
    ctx.fillStyle = stroke.color;
    const half = stroke.lineWidth * 0.5;
    for (let i = 0; i < stroke.sprayDensity; i++) {
      const angle = stroke.random() * 2 * Math.PI;
      const radius = stroke.random() * stroke.sprayRadius;
      ctx.fillRect(x + radius * Math.cos(angle) - half,
                   y + radius * Math.sin(angle) - half,
                   stroke.lineWidth, stroke.lineWidth);
    }
  }

  // The whole pen stroke as one path, painted once: the stroke has the same
  // opacity everywhere, also where it crosses itself (like geom_path() in
  // paindrawr). Separate strokes on top of each other do add up.
  function drawPen(ctx, stroke) {
    ctx.globalAlpha = stroke.alpha;
    ctx.lineCap = 'round';
    ctx.lineJoin = 'round';
    ctx.lineWidth = stroke.lineWidth;
    ctx.strokeStyle = stroke.color;
    ctx.beginPath();
    ctx.moveTo(stroke.p[0][0], stroke.p[0][1]);
    // Path starts with moveTo + lineTo of the first point, as in PainDrawEM.
    // (A one-point stroke is zero-length and shows nothing in Chromium.)
    stroke.p.forEach(function (pt) { ctx.lineTo(pt[0], pt[1]); });
    ctx.stroke();
  }

  // Background plus all strokes again (after undo, clear, new background)
  function redraw(pad) {
    const ctx = pad.ctx;
    const w = pad.canvas.width;
    const h = pad.canvas.height;
    ctx.globalAlpha = 1;
    ctx.clearRect(0, 0, w, h);
    if (pad.background) ctx.drawImage(pad.background, 0, 0, w, h);
    pad.strokes.forEach(function (stroke) {
      if (stroke.tool === 'pen') {
        drawPen(ctx, stroke);
      } else {
        stroke.random = seededRandom(stroke.seed); // same dots as first time
        stroke.p.forEach(function (pt) { drawSpray(ctx, stroke, pt[0], pt[1]); });
      }
    });
  }

  function undo(pad) {
    if (pad.current || pad.strokes.length === 0) return;
    pad.strokes.pop();
    redraw(pad);
    report(pad);
  }

  function clear(pad) {
    if (pad.current) return;
    pad.strokes = [];
    redraw(pad);
    report(pad);
  }

  // ---- Report the drawing to R --------------------------------------------
  // Only the PainDrawEM keys, in PainDrawEM order (no seed)
  function exportStroke(s) {
    return {
      strokeId: s.strokeId, drawInputType: s.drawInputType, tool: s.tool,
      color: s.color, lineWidth: s.lineWidth, sprayDensity: s.sprayDensity,
      sprayRadius: s.sprayRadius, alpha: s.alpha, scaleFactor: s.scaleFactor,
      dpr: s.dpr, p: s.p
    };
  }

  // After every stroke: the strokes only (small). R uses them for the status
  // text and to enable the download buttons.
  function report(pad) {
    updateButtons(pad);
    if (!window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue(pad.canvas.id + '_state', {
      token: pad.token,
      strokes: JSON.stringify(pad.strokes.map(exportStroke))
    });
  }

  // On a download click: strokes plus, for "png"/"zip", the PNG. The PNG is
  // the canvas at its own size (w x h), background included. R builds the
  // file and then asks us to click the hidden download link ("pd-download").
  function requestDownload(pad, kind) {
    if (pad.current || pad.strokes.length === 0) return;
    Shiny.setInputValue(pad.canvas.id + '_request', {
      token: pad.token,
      kind: kind,
      strokes: JSON.stringify(pad.strokes.map(exportStroke)),
      png: kind === 'json' ? null : pad.canvas.toDataURL('image/png'),
      nonce: Date.now()
    }, { priority: 'event' }); // "event": deliver even if identical to the last one
  }

  function updateButtons(pad) {
    const empty = pad.strokes.length === 0;
    document.querySelectorAll('[data-pd-action][data-pd-canvas="' + pad.canvas.id + '"]')
      .forEach(function (btn) { btn.disabled = empty; });
  }

  // ---- Messages from R -----------------------------------------------------
  function onShiny(fn) {
    if (window.Shiny && Shiny.addCustomMessageHandler) fn();
  }

  onShiny(function () {
    Shiny.addCustomMessageHandler('pd-settings', function (msg) {
      const pad = getPad(msg.id);
      if (pad) pad.settings = msg.settings;
    });

    // New background: resize the canvas, drop all strokes, load the image
    Shiny.addCustomMessageHandler('pd-background', function (msg) {
      const pad = getPad(msg.id);
      if (!pad) return;
      pad.token = msg.token;
      pad.strokes = [];
      pad.current = null;
      pad.pointerId = null;
      pad.background = null;
      pad.canvas.width = msg.w; // setting the size also blanks the canvas
      pad.canvas.height = msg.h;
      report(pad);

      const loadId = ++pad.loadCount;
      const img = new Image();
      img.onload = function () {
        if (loadId !== pad.loadCount) return; // a newer background was chosen
        pad.background = img;
        redraw(pad);
      };
      img.onerror = function () {
        if (loadId === pad.loadCount) window.alert(msg.error_text);
      };
      img.src = msg.url;
    });

    // Start a new drawing on the same background
    Shiny.addCustomMessageHandler('pd-clear', function (msg) {
      const pad = getPad(msg.id);
      if (!pad) return;
      pad.token = msg.token;
      pad.current = null;
      pad.pointerId = null;
      clear(pad);
    });

    // Enable/disable the download buttons
    Shiny.addCustomMessageHandler('pd-enable', function (msg) {
      [].concat(msg.ids).forEach(function (id) {
        const el = document.getElementById(id);
        if (el) el.disabled = !msg.enabled;
      });
    });

    // The file is ready on the server: click the hidden Shiny download link
    Shiny.addCustomMessageHandler('pd-download', function (msg) {
      const link = document.getElementById(msg.id);
      if (link) link.click();
    });

    // A colour picker as a Shiny input: input$<id> is "#rrggbb".
    // (An "input binding" tells Shiny how to read an HTML element's value.)
    const colorBinding = new Shiny.InputBinding();
    $.extend(colorBinding, {
      find: function (scope) { return $(scope).find('input.pd-color'); },
      getValue: function (el) { return el.value; },
      subscribe: function (el, callback) {
        $(el).on('input.pdColor change.pdColor', function () { callback(true); });
      },
      unsubscribe: function (el) { $(el).off('.pdColor'); },
      getRatePolicy: function () { return { policy: 'debounce', delay: 200 }; }
    });
    Shiny.inputBindings.register(colorBinding, 'paindraw.color');
  });

  // ---- Buttons -------------------------------------------------------------
  document.addEventListener('click', function (e) {
    const btn = e.target.closest('[data-pd-action]');
    if (!btn) return;
    const pad = getPad(btn.dataset.pdCanvas);
    if (!pad) return;
    if (btn.dataset.pdAction === 'undo') undo(pad);
    if (btn.dataset.pdAction === 'clear' && pad.strokes.length > 0 &&
        window.confirm(btn.dataset.pdConfirm)) clear(pad);
  });

  document.addEventListener('click', function (e) {
    const btn = e.target.closest('[data-pd-download]');
    if (!btn || btn.disabled) return;
    const pad = getPad(btn.dataset.pdCanvasId);
    if (pad) requestDownload(pad, btn.dataset.pdDownload);
  });
})();
