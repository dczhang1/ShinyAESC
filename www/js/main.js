/* main.js - JavaScript enhancements for ESCAPE */

(function() {
  'use strict';

  /* ============================================
     INITIALIZATION
     ============================================ */

  document.addEventListener('DOMContentLoaded', function() {
    initLucideIcons();
    initToastContainer();
    initDragDropEnhancement();
    initCarousel();
  });

  // Re-initialize after Shiny updates
  if (typeof Shiny !== 'undefined') {
    $(document).on('shiny:value', function() {
      // Small delay to let DOM update
      setTimeout(initLucideIcons, 50);
      setTimeout(initCarousel, 50);
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
      container.setAttribute('role', 'status');
      container.setAttribute('aria-live', 'polite');
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

    const iconWrap = document.createElement('span');
    iconWrap.style.color = config.bg;
    const iconEl = document.createElement('i');
    iconEl.setAttribute('data-lucide', config.icon);
    iconEl.style.width = '18px';
    iconEl.style.height = '18px';
    iconWrap.appendChild(iconEl);

    const messageEl = document.createElement('span');
    messageEl.style.flex = '1';
    messageEl.textContent = String(message);

    const closeBtn = document.createElement('button');
    closeBtn.style.cssText = 'background: none; border: none; color: #71717A; cursor: pointer; padding: 0;';
    const closeIcon = document.createElement('i');
    closeIcon.setAttribute('data-lucide', 'x');
    closeIcon.style.width = '16px';
    closeIcon.style.height = '16px';
    closeBtn.appendChild(closeIcon);

    toast.appendChild(iconWrap);
    toast.appendChild(messageEl);
    toast.appendChild(closeBtn);

    container.appendChild(toast);
    initLucideIcons();

    // Animate in
    requestAnimationFrame(() => {
      toast.style.transform = 'translateX(0)';
      toast.style.opacity = '1';
    });

    // Close button
    closeBtn.addEventListener('click', () => dismissToast(toast));

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

    Shiny.addCustomMessageHandler('focusGuidedWizard', function(data) {
      const step = Number(data && data.step ? data.step : 1);
      setTimeout(function() {
        const modal = document.querySelector('.guided-startup-modal');
        if (!modal) return;

        const targetSelector = step === 2
          ? '#guided_predictorVar'
          : '#guided_file, #guided_next, #guided_finish, #guided_skip';

        const target = modal.querySelector(targetSelector);
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
     CAROUSEL FOR USE CASES
     ============================================ */

  function initCarousel(retryCount = 0) {
    const carousel = document.querySelector('.use-cases-carousel');
    if (!carousel) {
      if (retryCount < 10) {
        console.log('Carousel not found, retrying in 500ms... (attempt ' + (retryCount + 1) + '/10)');
        setTimeout(function() {
          initCarousel(retryCount + 1);
        }, 500);
      } else {
        console.log('Carousel not found after 10 attempts, giving up');
      }
      return;
    }

    const slidesContainer = carousel.querySelector('.carousel-slides');
    if (!slidesContainer) {
      console.log('Slides container not found');
      return;
    }

    const slides = Array.from(slidesContainer.querySelectorAll('.use-case-card'));
    const prevBtn = carousel.querySelector('.carousel-prev');
    const nextBtn = carousel.querySelector('.carousel-next');
    const indicators = Array.from(carousel.querySelectorAll('.carousel-indicator'));

    if (carousel.dataset.initialized === 'true') {
      return;
    }
    carousel.dataset.initialized = 'true';
    console.log('Carousel init:', {
      slides: slides.length,
      prevBtn: !!prevBtn,
      nextBtn: !!nextBtn,
      indicators: indicators.length
    });

    if (slides.length === 0) {
      console.log('No slides found');
      return;
    }

    let currentSlide = 0;
    const totalSlides = slides.length;

    // Wrap each slide in a carousel-slide div
    slides.forEach(function(slide, index) {
      slide.classList.add('carousel-slide');
      if (index === 0) {
        slide.classList.add('active');
      }
    });

    function updateCarousel() {
      console.log('Updating carousel to slide:', currentSlide);
      // Update slides
      slides.forEach(function(slide, index) {
        if (index === currentSlide) {
          slide.classList.add('active');
        } else {
          slide.classList.remove('active');
        }
      });

      // Update indicators
      indicators.forEach(function(indicator, index) {
        if (index === currentSlide) {
          indicator.classList.add('active');
        } else {
          indicator.classList.remove('active');
        }
      });
    }

    function nextSlide() {
      console.log('Next slide clicked, current:', currentSlide, 'total:', totalSlides);
      currentSlide = (currentSlide + 1) % totalSlides;
      updateCarousel();
    }

    function prevSlide() {
      console.log('Prev slide clicked, current:', currentSlide, 'total:', totalSlides);
      currentSlide = (currentSlide - 1 + totalSlides) % totalSlides;
      updateCarousel();
    }

    function goToSlide(index) {
      console.log('Go to slide:', index);
      currentSlide = index;
      updateCarousel();
    }

    // Event listeners
    if (prevBtn) {
      console.log('Attaching prev button listener');
      prevBtn.addEventListener('click', function(e) {
        e.preventDefault();
        e.stopPropagation();
        console.log('Prev button clicked');
        prevSlide();
      });
    } else {
      console.log('Prev button not found');
    }

    if (nextBtn) {
      console.log('Attaching next button listener');
      nextBtn.addEventListener('click', function(e) {
        e.preventDefault();
        e.stopPropagation();
        console.log('Next button clicked');
        nextSlide();
      });
    } else {
      console.log('Next button not found');
    }

    indicators.forEach(function(indicator, index) {
      console.log('Attaching indicator listener for slide', index);
      indicator.addEventListener('click', function(e) {
        e.preventDefault();
        e.stopPropagation();
        const slideIndex = parseInt(this.getAttribute('data-slide'), 10);
        console.log('Indicator clicked for slide', slideIndex);
        goToSlide(slideIndex);
      });
    });

    // Auto-advance every 5 seconds
    const reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    let autoAdvance = reduceMotion ? null : setInterval(nextSlide, 5000);

    // Pause on hover
    carousel.addEventListener('mouseenter', function() {
      if (autoAdvance) clearInterval(autoAdvance);
    });

    carousel.addEventListener('mouseleave', function() {
      if (!reduceMotion) autoAdvance = setInterval(nextSlide, 5000);
    });

    // Keyboard navigation
    document.addEventListener('keydown', function(e) {
      if (document.activeElement.closest('.use-cases-carousel')) {
        if (e.key === 'ArrowLeft') {
          prevSlide();
        } else if (e.key === 'ArrowRight') {
          nextSlide();
        }
      }
    });
  }

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
