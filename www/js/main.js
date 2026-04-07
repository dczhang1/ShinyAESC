/* main.js - JavaScript enhancements for ESCAPE */

(function() {
  'use strict';

  document.addEventListener('DOMContentLoaded', function() {
    initLucideIcons();
    initToastContainer();
    initDragDropEnhancement();
    initScrollAnimations();
    initMiniIconWave();
    initHeroParticles();
    initInsightMorph();
    initLivePreviewChartReveal();
    initLandingCiteBlocks();
  });

  if (typeof Shiny !== 'undefined') {
    var shinyValueInitTimer = null;
    $(document).on('shiny:value', function() {
      if (shinyValueInitTimer) {
        clearTimeout(shinyValueInitTimer);
      }
      shinyValueInitTimer = setTimeout(function() {
        initLucideIcons();
        initScrollAnimations();
        initMiniIconWave();
        initHeroParticles();
        initInsightMorph();
        initLivePreviewChartReveal();
        initLandingCiteBlocks();
        initDragDropEnhancement();
      }, 80);
    });

    Shiny.addCustomMessageHandler('initIcons', function(data) {
      setTimeout(initLucideIcons, 100);
    });
  }

  function initLucideIcons() {
    if (typeof lucide !== 'undefined') {
      lucide.createIcons();
    }
  }

  function initToastContainer() {
    if (!document.getElementById('toast-container')) {
      var container = document.createElement('div');
      container.id = 'toast-container';
      container.className = 'toast-container';
      container.setAttribute('role', 'status');
      container.setAttribute('aria-live', 'polite');
      container.style.cssText =
        'position:fixed;bottom:1rem;right:1rem;z-index:9999;' +
        'display:flex;flex-direction:column;gap:0.5rem;';
      document.body.appendChild(container);
    }
  }

  function showToast(message, type, duration) {
    duration = duration || 4000;
    var container = document.getElementById('toast-container');
    if (!container) return;

    var toast = document.createElement('div');
    toast.className = 'toast toast-' + type;

    var colors = {
      info: { bg: '#2d5a3d', icon: 'info' },
      success: { bg: '#3d7a52', icon: 'check-circle' },
      warning: { bg: '#d4af37', icon: 'alert-triangle' },
      error: { bg: '#b4534a', icon: 'x-circle' }
    };

    var config = colors[type] || colors.info;

    toast.style.cssText =
      'display:flex;align-items:center;gap:0.75rem;padding:0.75rem 1rem;' +
      'background:#1a1a1a;color:white;border-radius:0.5rem;' +
      'box-shadow:0 4px 12px rgba(0,0,0,0.15);font-size:0.875rem;' +
      'font-family:system-ui,sans-serif;transform:translateX(100%);opacity:0;' +
      'transition:all 200ms ease-out;max-width:350px;';

    var iconWrap = document.createElement('span');
    iconWrap.style.color = config.bg;
    var iconEl = document.createElement('i');
    iconEl.setAttribute('data-lucide', config.icon);
    iconEl.style.width = '18px';
    iconEl.style.height = '18px';
    iconWrap.appendChild(iconEl);

    var messageEl = document.createElement('span');
    messageEl.style.flex = '1';
    messageEl.textContent = String(message);

    var closeBtn = document.createElement('button');
    closeBtn.style.cssText = 'background:none;border:none;color:#71717A;cursor:pointer;padding:0;';
    var closeIcon = document.createElement('i');
    closeIcon.setAttribute('data-lucide', 'x');
    closeIcon.style.width = '16px';
    closeIcon.style.height = '16px';
    closeBtn.appendChild(closeIcon);

    toast.appendChild(iconWrap);
    toast.appendChild(messageEl);
    toast.appendChild(closeBtn);

    container.appendChild(toast);
    initLucideIcons();

    requestAnimationFrame(function() {
      toast.style.transform = 'translateX(0)';
      toast.style.opacity = '1';
    });

    closeBtn.addEventListener('click', function() { dismissToast(toast); });

    if (duration > 0) {
      setTimeout(function() { dismissToast(toast); }, duration);
    }

    return toast;
  }

  function dismissToast(toast) {
    toast.style.transform = 'translateX(100%)';
    toast.style.opacity = '0';
    setTimeout(function() { toast.remove(); }, 200);
  }

  window.showToast = showToast;

  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('showToast', function(data) {
      showToast(data.message, data.type, data.duration);
    });

    Shiny.addCustomMessageHandler('focusGuidedWizard', function(data) {
      var step = Number(data && data.step ? data.step : 1);
      setTimeout(function() {
        var modal = document.querySelector('.guided-startup-modal');
        if (!modal) return;
        var targetSelector = step === 2
          ? '#guided_predictorVar'
          : '#guided_file, #guided_next, #guided_finish, #guided_skip';
        var target = modal.querySelector(targetSelector);
        if (target && typeof target.focus === 'function') {
          target.focus();
        }
      }, 80);
    });
  }

  function initDragDropEnhancement() {
    var fileInputs = document.querySelectorAll('.shiny-input-container input[type="file"]');
    fileInputs.forEach(function(input) {
      var container = input.closest('.shiny-input-container');
      if (!container) return;
      var dropZone = container.querySelector('.input-group') || container;
      if (dropZone.getAttribute('data-dragdrop-init') === '1') return;
      dropZone.setAttribute('data-dragdrop-init', '1');

      ['dragenter', 'dragover'].forEach(function(eventName) {
        dropZone.addEventListener(eventName, function(e) {
          e.preventDefault();
          e.stopPropagation();
          dropZone.classList.add('drag-over');
        });
      });

      ['dragleave', 'drop'].forEach(function(eventName) {
        dropZone.addEventListener(eventName, function(e) {
          e.preventDefault();
          e.stopPropagation();
          dropZone.classList.remove('drag-over');
        });
      });
    });

    if (!document.getElementById('escape-dragdrop-style')) {
      var style = document.createElement('style');
      style.id = 'escape-dragdrop-style';
      style.textContent = '.drag-over{border-color:#2d5a3d!important;background:rgba(45,90,61,0.08)!important;}';
      document.head.appendChild(style);
    }
  }

  function animateNumber(element, start, end, duration) {
    duration = duration || 500;
    var startTime = performance.now();
    function update(currentTime) {
      var elapsed = currentTime - startTime;
      var progress = Math.min(elapsed / duration, 1);
      var eased = 1 - Math.pow(1 - progress, 3);
      var current = start + (end - start) * eased;
      element.textContent = current.toFixed(3);
      if (progress < 1) requestAnimationFrame(update);
    }
    requestAnimationFrame(update);
  }

  window.animateNumber = animateNumber;

  function initMiniIconWave() {
    var container = document.querySelector('.transformation-icon-array');
    if (!container) return;
    var icons = container.querySelectorAll('.mini-icon');
    icons.forEach(function(icon, index) {
      icon.style.animationDelay = (index * 0.02) + 's';
    });
  }

  var scrollAnimObserver = null;

  function initScrollAnimations() {
    if (scrollAnimObserver) {
      scrollAnimObserver.disconnect();
      scrollAnimObserver = null;
    }
    scrollAnimObserver = new IntersectionObserver(
      function(entries) {
        entries.forEach(function(entry) {
          if (entry.isIntersecting) {
            entry.target.classList.add('is-visible');
            entry.target.classList.add('fade-in');
            scrollAnimObserver.unobserve(entry.target);
          }
        });
      },
      { threshold: 0.2 }
    );

    document.querySelectorAll(
      '.feature-card, .step-card, .scroll-section'
    ).forEach(function(el) {
      if (!el.classList.contains('is-visible')) {
        scrollAnimObserver.observe(el);
      }
    });
  }

  function initHeroParticles() {
    var canvas = document.getElementById('hero-particles-canvas');
    if (!canvas || !canvas.getContext) return;
    if (canvas.getAttribute('data-particles-on') === '1') return;
    canvas.setAttribute('data-particles-on', '1');

    var ctx = canvas.getContext('2d');
    if (!ctx) return;

    var particles = [];
    var n = 42;
    var w = 0;
    var h = 0;

    function resize() {
      var section = canvas.closest('.hero-section--observatory');
      if (!section) return;
      w = section.offsetWidth;
      h = section.offsetHeight;
      if (w < 1 || h < 1) return;
      canvas.width = w;
      canvas.height = h;
      particles.length = 0;
      for (var i = 0; i < n; i++) {
        particles.push({
          x: Math.random() * w,
          y: Math.random() * h,
          r: 0.6 + Math.random() * 1.8,
          vx: (Math.random() - 0.5) * 0.12,
          vy: (Math.random() - 0.5) * 0.12,
          a: 0.15 + Math.random() * 0.35
        });
      }
    }

    function tick() {
      if (canvas.getAttribute('data-particles-on') !== '1') return;
      if (w < 1) {
        requestAnimationFrame(tick);
        return;
      }
      ctx.clearRect(0, 0, w, h);
      for (var i = 0; i < particles.length; i++) {
        var p = particles[i];
        p.x += p.vx;
        p.y += p.vy;
        if (p.x < -5) p.x = w + 5;
        if (p.x > w + 5) p.x = -5;
        if (p.y < -5) p.y = h + 5;
        if (p.y > h + 5) p.y = -5;
        ctx.beginPath();
        ctx.arc(p.x, p.y, p.r, 0, Math.PI * 2);
        ctx.fillStyle = 'rgba(212, 175, 55, ' + p.a + ')';
        ctx.fill();
      }
      requestAnimationFrame(tick);
    }

    if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
      return;
    }

    resize();
    window.addEventListener('resize', resize);
    requestAnimationFrame(tick);
  }

  var insightMorphTimer = null;

  function initInsightMorph() {
    var morph = document.querySelector('.insight-morph');
    if (!morph || morph.getAttribute('data-morph-bound') === '1') return;
    morph.setAttribute('data-morph-bound', '1');

    if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
      return;
    }

    var started = false;

    function startFlipping() {
      if (started) return;
      started = true;
      if (insightMorphTimer) clearInterval(insightMorphTimer);
      insightMorphTimer = setInterval(function() {
        morph.classList.toggle('insight-morph--flip');
      }, 3200);
    }

    var io = new IntersectionObserver(
      function(entries) {
        entries.forEach(function(entry) {
          if (entry.isIntersecting) {
            startFlipping();
            io.disconnect();
          }
        });
      },
      { threshold: 0.25 }
    );
    io.observe(morph);
  }

  function initLivePreviewChartReveal() {
    var chart = document.querySelector('.live-preview-chart');
    if (!chart || chart.getAttribute('data-chart-io') === '1') return;
    chart.setAttribute('data-chart-io', '1');

    var io = new IntersectionObserver(
      function(entries) {
        entries.forEach(function(entry) {
          if (entry.isIntersecting) {
            chart.classList.add('is-chart-revealed');
            io.disconnect();
          }
        });
      },
      { threshold: 0.2 }
    );
    io.observe(chart);
  }

  function initLandingCiteBlocks() {
    document.querySelectorAll('.landing-cite-card--combined').forEach(function(card) {
      if (card.getAttribute('data-cite-init') === '1') return;
      card.setAttribute('data-cite-init', '1');

      var paperBar = card.querySelector('.landing-cite-paper-bar');
      var formatBar = card.querySelector('.landing-cite-format-bar');
      var panels = card.querySelectorAll('.landing-cite-panel');
      var copyBtn = card.querySelector('.landing-cite-copy');
      if (!formatBar || !panels.length || !copyBtn) return;

      function activePaperId() {
        if (!paperBar) return 'fpsyg';
        var b = paperBar.querySelector('.landing-cite-paper-btn.is-active');
        return b ? b.getAttribute('data-paper') : 'fpsyg';
      }

      function activeFormatId() {
        var b = formatBar.querySelector('.landing-cite-format-btn.is-active');
        return b ? b.getAttribute('data-format') : 'bibtex';
      }

      function syncPanels() {
        var pid = activePaperId();
        var fid = activeFormatId();
        panels.forEach(function(p) {
          var on =
            p.getAttribute('data-paper') === pid &&
            p.getAttribute('data-format') === fid;
          p.classList.toggle('is-active', on);
        });
      }

      if (paperBar) {
        paperBar.addEventListener('click', function(e) {
          var btn = e.target.closest('.landing-cite-paper-btn');
          if (!btn || !paperBar.contains(btn)) return;
          paperBar.querySelectorAll('.landing-cite-paper-btn').forEach(function(b) {
            var on = b === btn;
            b.classList.toggle('is-active', on);
            b.setAttribute('aria-selected', on ? 'true' : 'false');
          });
          syncPanels();
        });
      }

      formatBar.addEventListener('click', function(e) {
        var btn = e.target.closest('.landing-cite-format-btn');
        if (!btn || !formatBar.contains(btn)) return;
        formatBar.querySelectorAll('.landing-cite-format-btn').forEach(function(b) {
          var on = b === btn;
          b.classList.toggle('is-active', on);
          b.setAttribute('aria-selected', on ? 'true' : 'false');
        });
        syncPanels();
      });

      copyBtn.addEventListener('click', function() {
        var active = card.querySelector('.landing-cite-panel.is-active');
        if (!active) return;
        var text = active.textContent || '';

        function done() {
          if (typeof showToast === 'function') {
            showToast('Citation copied to clipboard', 'success', 2800);
          }
        }

        function fallbackCopy() {
          var ta = document.createElement('textarea');
          ta.value = text;
          ta.setAttribute('readonly', '');
          ta.style.position = 'absolute';
          ta.style.left = '-9999px';
          document.body.appendChild(ta);
          ta.select();
          try {
            if (document.execCommand('copy')) {
              done();
            }
          } catch (err) {}
          document.body.removeChild(ta);
        }

        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(text).then(done).catch(function() {
            fallbackCopy();
          });
        } else {
          fallbackCopy();
        }
      });

      syncPanels();
    });
  }

})();
