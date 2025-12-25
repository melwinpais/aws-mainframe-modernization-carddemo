<template>
  <div v-if="message" :class="['error-message', `error-${type}`]" role="alert">
    <div class="error-icon">
      <span v-if="type === 'error'">⚠️</span>
      <span v-else-if="type === 'warning'">⚡</span>
      <span v-else-if="type === 'info'">ℹ️</span>
      <span v-else-if="type === 'success'">✓</span>
    </div>
    <div class="error-content">
      <p class="error-text">{{ message }}</p>
      <ul v-if="fieldErrors && fieldErrors.length > 0" class="field-errors">
        <li v-for="(error, index) in fieldErrors" :key="index" class="field-error">
          <strong>{{ error.field }}:</strong> {{ error.message }}
        </li>
      </ul>
    </div>
    <button v-if="dismissible" @click="handleDismiss" class="dismiss-button" aria-label="Dismiss">
      ×
    </button>
  </div>
</template>

<script setup>
import { defineProps, defineEmits } from 'vue'

const props = defineProps({
  message: {
    type: String,
    default: ''
  },
  type: {
    type: String,
    default: 'error',
    validator: (value) => ['error', 'warning', 'info', 'success'].includes(value)
  },
  fieldErrors: {
    type: Array,
    default: () => []
  },
  dismissible: {
    type: Boolean,
    default: true
  }
})

const emit = defineEmits(['dismiss'])

const handleDismiss = () => {
  emit('dismiss')
}
</script>

<style scoped>
.error-message {
  display: flex;
  align-items: flex-start;
  gap: 0.75rem;
  padding: 1rem;
  border-radius: 4px;
  margin-bottom: 1rem;
  border-left: 4px solid;
}

.error-error {
  background-color: #fee;
  border-left-color: #e74c3c;
  color: #c0392b;
}

.error-warning {
  background-color: #fef5e7;
  border-left-color: #f39c12;
  color: #d68910;
}

.error-info {
  background-color: #ebf5fb;
  border-left-color: #3498db;
  color: #2874a6;
}

.error-success {
  background-color: #eafaf1;
  border-left-color: #27ae60;
  color: #1e8449;
}

.error-icon {
  font-size: 1.25rem;
  flex-shrink: 0;
}

.error-content {
  flex: 1;
}

.error-text {
  margin: 0;
  font-weight: 500;
}

.field-errors {
  margin: 0.5rem 0 0 0;
  padding-left: 1.25rem;
  list-style-type: disc;
}

.field-error {
  margin: 0.25rem 0;
  font-size: 0.9rem;
}

.dismiss-button {
  background: none;
  border: none;
  font-size: 1.5rem;
  line-height: 1;
  cursor: pointer;
  padding: 0;
  color: inherit;
  opacity: 0.6;
  transition: opacity 0.2s;
  flex-shrink: 0;
}

.dismiss-button:hover {
  opacity: 1;
}

.dismiss-button:focus {
  outline: 2px solid currentColor;
  outline-offset: 2px;
}
</style>
