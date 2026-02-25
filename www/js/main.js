/* main.js - JavaScript enhancements for ShinyAESC */

(function() {
  'use strict';

  /* ============================================
     INITIALIZATION
     ============================================ */

  document.addEventListener('DOMContentLoaded', function() {
    initLucideIcons();
    initToastContainer();
    initDragDropEnhancement();
  });

  // Re-initialize after Shiny updates
  if (typeof Shiny !== 'undefined') {
    $(document).on('shiny:value', function() {
      // Small delay to let DOM update
      setTimeout(initLucideIcons, 50);
    });

    // Custom message handler for icon re-init
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
    // Create toast container if it doesn't exist
    if (!document.getElementById('toast-container')) {
      const container = document.createElement('div');
      container.id = 'toast-container';
      container.className = 'toast-container';
      container.style.cssText = `
        position: fixed;
        bottom: 1rem;
        right: 1rem;
        z-index: 9999;
        display: flex;
        flex-direction: column;
        gap: 0.5rem;
      `;
      document.body.appendChild(container);
    }
  }

  function showToast(message, type = 'info', duration = 4000) {
    const container = document.getElementById('toast-container');
    if (!container) return;

    const toast = document.createElement('div');
    toast.className = `toast toast-${type}`;

    const colors = {
      info: { bg: '#5E6AD2', icon: 'info' },
      success: { bg: '#3CCB7F', icon: 'check-circle' },
      warning: { bg: '#FFBB38', icon: 'alert-triangle' },
      error: { bg: '#F87171', icon: 'x-circle' }
    };

    const config = colors[type] || colors.info;

    toast.style.cssText = `
      display: flex;
      align-items: center;
      gap: 0.75rem;
      padding: 0.75rem 1rem;
      background: #18181B;
      color: white;
      border-radius: 0.5rem;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
      font-size: 0.875rem;
      font-family: Inter, sans-serif;
      transform: translateX(100%);
      opacity: 0;
      transition: all 200ms ease-out;
      max-width: 350px;
    `;

    toast.innerHTML = `
      <span style="color: ${config.bg};">
        <i data-lucide="${config.icon}" style="width: 18px; height: 18px;"></i>
      </span>
      <span style="flex: 1;">${message}</span>
      <button style="background: none; border: none; color: #71717A; cursor: pointer; padding: 0;">
        <i data-lucide="x" style="width: 16px; height: 16px;"></i>
      </button>
    `;

    container.appendChild(toast);
    initLucideIcons();

    // Animate in
    requestAnimationFrame(() => {
      toast.style.transform = 'translateX(0)';
      toast.style.opacity = '1';
    });

    // Close button
    toast.querySelector('button').addEventListener('click', () => dismissToast(toast));

    // Auto dismiss
    if (duration > 0) {
      setTimeout(() => dismissToast(toast), duration);
    }

    return toast;
  }

  function dismissToast(toast) {
    toast.style.transform = 'translateX(100%)';
    toast.style.opacity = '0';
    setTimeout(() => toast.remove(), 200);
  }

  // Expose to global scope for Shiny
  window.showToast = showToast;

  // Shiny custom message handler
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('showToast', function(data) {
      showToast(data.message, data.type, data.duration);
    });
  }

  /* ============================================
     DRAG & DROP FILE UPLOAD ENHANCEMENT
     ============================================ */

  function initDragDropEnhancement() {
    const fileInputs = document.querySelectorAll('.shiny-input-container input[type="file"]');

    fileInputs.forEach(function(input) {
      const container = input.closest('.shiny-input-container');
      if (!container) return;

      const dropZone = container.querySelector('.input-group') || container;

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

    // Add drag-over styles
    const style = document.createElement('style');
    style.textContent = `
      .drag-over {
        border-color: #5E6AD2 !important;
        background: rgba(94, 106, 210, 0.1) !important;
      }
    `;
    document.head.appendChild(style);
  }

  /* ============================================
     NUMBER ANIMATION
     ============================================ */

  function animateNumber(element, start, end, duration = 500) {
    const startTime = performance.now();

    function update(currentTime) {
      const elapsed = currentTime - startTime;
      const progress = Math.min(elapsed / duration, 1);

      // Ease out cubic
      const eased = 1 - Math.pow(1 - progress, 3);
      const current = start + (end - start) * eased;

      element.textContent = current.toFixed(3);

      if (progress < 1) {
        requestAnimationFrame(update);
      }
    }

    requestAnimationFrame(update);
  }

  window.animateNumber = animateNumber;

  /* ============================================
     INTERSECTION OBSERVER FOR ANIMATIONS
     ============================================ */

  function initScrollAnimations() {
    const observer = new IntersectionObserver(
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

    document.querySelectorAll('.card, .tab-pane').forEach(function(el) {
      observer.observe(el);
    });
  }

  // Initialize on load
  document.addEventListener('DOMContentLoaded', initScrollAnimations);

})();
