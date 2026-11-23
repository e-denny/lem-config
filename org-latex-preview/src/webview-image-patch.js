// Teach the webview frontend to paint images.
//
// The frontend's canvas surface has no image path: lem-server sends it character
// cells and it draws text, so an image-drawing object (an attribute carrying
// :image, :width, :height and :ascent) arrives as nothing but its placeholder
// spaces.  This script patches the surface class of the running client so that
// a "put" whose attribute carries an image paints the image in the cells the
// server reserved for it, and reports the cell metrics back to Lisp.
//
// It is evaluated in the page through the `js-eval` frontend call, in the scope
// of a BaseSurface method: `this` is the canvas surface of the window whose view
// was named, and the module the client was built from is in scope.  It does not
// use any class name, only the surface instance it is given, so it keeps working
// against the minified bundle.
(() => {
  const surface = this;
  const proto = Object.getPrototypeOf(surface);
  const option = surface.editor.option;

  // The metrics are what Lisp needs to place the images, and they are cheap to
  // repeat: a Lisp that reloads this extension asks for the patch again, and
  // needs the answer even when the client was patched long ago.
  const reportMetrics = () => {
    try {
      surface.editor.notifyToServer('org-latex-preview/metrics', {
        cellWidth: option.fontWidth,
        cellHeight: option.fontHeight,
        fontSize: option.fontSize,
      });
    } catch (error) {
      // a closed window has nothing to measure
    }
  };

  if (proto.__lemImagePatch) {
    reportMetrics();
    return 'already';
  }

  // The surface method that paints a run of cells is `drawText` in this build;
  // older bundles had the same method under the name `print`.  Only the near-
  // est one to the instance is wanted: the base class carries a no-op of the
  // same name, which the canvas subclass overrides.
  let owner = null;
  let method = null;
  for (const candidate of ['drawText', 'print']) {
    let chain = Object.getPrototypeOf(surface);
    for (let i = 0; i < 8 && chain; i++) {
      if (Object.prototype.hasOwnProperty.call(chain, candidate)) {
        owner = chain;
        method = candidate;
        break;
      }
      chain = Object.getPrototypeOf(chain);
    }
    if (owner) {
      break;
    }
  }
  if (!owner) {
    return 'no-draw-method';
  }
  const originalDraw = owner[method];

  // A glyph starts at the top of its cell (drawText sets textBaseline to
  // 'top'), so the text baseline sits one font ascent below the cell top.
  const baseline = (() => {
    const ctx = document.createElement('canvas').getContext('2d');
    ctx.font = option.font;
    ctx.textBaseline = 'alphabetic';
    const metrics = ctx.measureText('Mg');
    return metrics.fontBoundingBoxAscent || option.fontSize * 0.8;
  })();

  const images = new Map();
  let redrawPending = false;

  // An image arrives on screen only after it has decoded, which is after the
  // frame that asked for it.  Ask Lem for another frame, once per burst.
  const requestRedraw = () => {
    if (redrawPending) {
      return;
    }
    redrawPending = true;
    setTimeout(() => {
      redrawPending = false;
      try {
        surface.editor.notifyToServer('org-latex-preview/redraw', {});
      } catch (error) {
        // a closed window has nothing to redraw
      }
    }, 40);
  };

  const imageFor = (source) => {
    let entry = images.get(source);
    if (!entry) {
      const image = new Image();
      image.onload = requestRedraw;
      image.onerror = requestRedraw;
      image.src = '/local' + source;
      entry = { image };
      images.set(source, entry);
    }
    return entry.image;
  };

  owner[method] = function (x, y, text, textWidth, attribute, font) {
    if (!attribute || !attribute.image) {
      return originalDraw.call(this, x, y, text, textWidth, attribute, font);
    }
    const self = this;
    const gx = x * option.fontWidth;
    const gy = y * option.fontHeight;
    const width = attribute.imageWidth || option.fontWidth;
    const height = attribute.imageHeight || option.fontHeight;
    const ascent = (attribute.imageAscent === null || attribute.imageAscent === undefined
      ? 90
      : attribute.imageAscent) / 100;
    const top = gy + baseline - ascent * height;
    const background = attribute.background || option.background;
    const image = imageFor(attribute.image);

    self.drawingQueue.push((ctx) => {
      ctx.fillStyle = background;
      ctx.fillRect(gx, gy, width, height);
      if (image.complete && image.naturalWidth > 0) {
        ctx.drawImage(image, gx, top, width, height);
        window.__lemImageDraws = (window.__lemImageDraws || 0) + 1;
      }
    });
  };

  proto.__lemImagePatch = true;
  reportMetrics();
  return 'patched';
})()
