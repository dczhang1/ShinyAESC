/* main.js - JavaScript enhancements for ESCAPE */

(function() {
  'use strict';

  var carouselTimer = null;

  document.addEventListener('DOMContentLoaded', function() {
    initLucideIcons();
    initToastContainer();
    initDragDropEnhancement();
    initScrollAnimations();
    initMiniIconWave();
    initCarousel();
  });

  if (typeof Shiny !== 'undefined') {
    $(document).on('shiny:value', function() {
      setTimeout(initLucideIcons, 50);
      setTimeout(initScrollAnimations, 50);
      setTimeout(initMiniIconWave, 50);
      setTimeout(initCarousel, 50);
    });

    Shiny.addCustomMessageHandler('initIcons', function(data) {
      setTimeout(initLucideIcons, 100);
    });
  }

  /* ============================================
     LUCIDE ICONS
     ============================================ */

  function initLucideIcons() {
    if (typeof lucide !== 'undefined') {
      lucide.createIcons();
    }
  }

  /* ============================================
     TOAST NOTIFICATIONS
     ============================================ */

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
      info: { bg: '#5E6AD2', icon: 'info' },
      success: { bg: '#3CCB7F', icon: 'check-circle' },
      warning: { bg: '#FFBB38', icon: 'alert-triangle' },
      error: { bg: '#F87171', icon: 'x-circle' }
    };

    var config = colors[type] || colors.info;

    toast.style.cssText =
      'display:flex;align-items:center;gap:0.75rem;padding:0.75rem 1rem;' +
      'background:#18181B;color:white;border-radius:0.5rem;' +
      'box-shadow:0 4px 12px rgba(0,0,0,0.15);font-size:0.875rem;' +
      'font-family:Inter,sans-serif;transform:translateX(100%);opacity:0;' +
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

  /* ============================================
     DRAG & DROP FILE UPLOAD ENHANCEMENT
     ============================================ */

  function initDragDropEnhancement() {
    var fileInputs = document.querySelectorAll('.shiny-input-container input[type="file"]');
    fileInputs.forEach(function(input) {
      var container = input.closest('.shiny-input-container');
      if (!container) return;
      var dropZone = container.querySelector('.input-group') || container;

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

    var style = document.createElement('style');
    style.textContent = '.drag-over{border-color:#5E6AD2!important;background:rgba(94,106,210,0.1)!important;}';
    document.head.appendChild(style);
  }

  /* ============================================
     NUMBER ANIMATION
     ============================================ */

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

  /* ============================================
     MINI ICON WAVE ANIMATION
     ============================================ */

  function initMiniIconWave() {
    var container = document.querySelector('.transformation-icon-array');
    if (!container) return;
    var icons = container.querySelectorAll('.mini-icon');
    icons.forEach(function(icon, index) {
      icon.style.animationDelay = (index * 0.02) + 's';
    });
  }

  /* ============================================
     INTERSECTION OBSERVER FOR ANIMATIONS
     ============================================ */

  function initScrollAnimations() {
    var observer = new IntersectionObserver(
      function(entries) {
        entries.forEach(function(entry) {
          if (entry.isIntersecting) {
            entry.target.classList.add('fade-in');
            observer.unobserve(entry.target);
          }
        });
      },
      { threshold: 0.1 }
    );
    document.querySelectorAll('.feature-card, .step-card, .use-case-card, .problem-callout').forEach(function(el) {
      observer.observe(el);
    });
  }

  /* ============================================
     AUTO-ADVANCING CAROUSEL
     ============================================ */

  function initCarousel() {
    var carousel = document.querySelector('.use-cases-carousel');
    if (!carousel) return;

    var track = carousel.querySelector('.carousel-track');
    var slides = carousel.querySelectorAll('.use-case-card');
    var prevBtn = carousel.querySelector('.carousel-prev');
    var nextBtn = carousel.querySelector('.carousel-next');
    var dots = carousel.querySelectorAll('.carousel-dot');

    if (!track || slides.length === 0) return;

    var current = 0;
    var total = slides.length;
    var interval = 6000;

    function goTo(index) {
      if (index < 0) index = total - 1;
      if (index >= total) index = 0;
      current = index;
      track.style.transform = 'translateX(-' + (current * 100) + '%)';
      dots.forEach(function(dot, i) {
        dot.classList.toggle('active', i === current);
      });
    }

    function next() { goTo(current + 1); }
    function prev() { goTo(current - 1); }

    if (nextBtn) nextBtn.addEventListener('click', function() {
      next();
      resetTimer();
    });

    if (prevBtn) prevBtn.addEventListener('click', function() {
      prev();
      resetTimer();
    });

    dots.forEach(function(dot, i) {
      dot.addEventListener('click', function() {
        goTo(i);
        resetTimer();
      });
    });

    function startTimer() {
      if (carouselTimer) clearInterval(carouselTimer);
      carouselTimer = setInterval(next, interval);
    }

    function resetTimer() {
      if (carouselTimer) clearInterval(carouselTimer);
      startTimer();
    }

    carousel.addEventListener('mouseenter', function() {
      if (carouselTimer) clearInterval(carouselTimer);
    });

    carousel.addEventListener('mouseleave', function() {
      startTimer();
    });

    goTo(0);
    startTimer();
  }

})();
