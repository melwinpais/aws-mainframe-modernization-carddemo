<template>
  <div :class="['loading-spinner', { 'loading-overlay': overlay }]">
    <div class="spinner-container">
      <div :class="['spinner', `spinner-${size}`]"></div>
      <p v-if="message" class="loading-message">{{ message }}</p>
    </div>
  </div>
</template>

<script setup>
import { defineProps } from 'vue'

const props = defineProps({
  message: {
    type: String,
    default: ''
  },
  size: {
    type: String,
    default: 'medium',
    validator: (value) => ['small', 'medium', 'large'].includes(value)
  },
  overlay: {
    type: Boolean,
    default: false
  }
})
</script>

<style scoped>
.loading-spinner {
  display: flex;
  justify-content: center;
  align-items: center;
  padding: 2rem;
}

.loading-overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background-color: rgba(0, 0, 0, 0.5);
  z-index: 9999;
  padding: 0;
}

.spinner-container {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 1rem;
}

.spinner {
  border-radius: 50%;
  border-style: solid;
  border-color: #3498db #3498db #3498db transparent;
  animation: spin 1s linear infinite;
}

.spinner-small {
  width: 20px;
  height: 20px;
  border-width: 2px;
}

.spinner-medium {
  width: 40px;
  height: 40px;
  border-width: 4px;
}

.spinner-large {
  width: 60px;
  height: 60px;
  border-width: 6px;
}

.loading-overlay .spinner {
  border-color: #fff #fff #fff transparent;
}

.loading-message {
  margin: 0;
  color: #2c3e50;
  font-size: 0.9rem;
}

.loading-overlay .loading-message {
  color: white;
  font-size: 1rem;
}

@keyframes spin {
  0% {
    transform: rotate(0deg);
  }
  100% {
    transform: rotate(360deg);
  }
}
</style>
