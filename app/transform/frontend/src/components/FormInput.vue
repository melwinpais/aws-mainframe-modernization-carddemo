<template>
  <div class="form-input">
    <label v-if="label" :for="inputId" class="input-label">
      {{ label }}
      <span v-if="required" class="required-indicator">*</span>
    </label>
    <input
      :id="inputId"
      :type="type"
      :value="modelValue"
      @input="handleInput"
      @blur="handleBlur"
      :placeholder="placeholder"
      :disabled="disabled"
      :readonly="readonly"
      :required="required"
      :maxlength="maxlength"
      :min="min"
      :max="max"
      :step="step"
      :class="['input-field', { 'input-error': error, 'input-disabled': disabled }]"
      :aria-invalid="!!error"
      :aria-describedby="error ? `${inputId}-error` : undefined"
    />
    <p v-if="error" :id="`${inputId}-error`" class="error-text" role="alert">
      {{ error }}
    </p>
    <p v-else-if="hint" class="hint-text">
      {{ hint }}
    </p>
  </div>
</template>

<script setup>
import { defineProps, defineEmits, computed } from 'vue'

const props = defineProps({
  modelValue: {
    type: [String, Number],
    default: ''
  },
  label: {
    type: String,
    default: ''
  },
  type: {
    type: String,
    default: 'text',
    validator: (value) => ['text', 'password', 'email', 'number', 'tel', 'date'].includes(value)
  },
  placeholder: {
    type: String,
    default: ''
  },
  error: {
    type: String,
    default: ''
  },
  hint: {
    type: String,
    default: ''
  },
  disabled: {
    type: Boolean,
    default: false
  },
  readonly: {
    type: Boolean,
    default: false
  },
  required: {
    type: Boolean,
    default: false
  },
  maxlength: {
    type: Number,
    default: undefined
  },
  min: {
    type: [Number, String],
    default: undefined
  },
  max: {
    type: [Number, String],
    default: undefined
  },
  step: {
    type: [Number, String],
    default: undefined
  },
  id: {
    type: String,
    default: ''
  }
})

const emit = defineEmits(['update:modelValue', 'blur'])

const inputId = computed(() => props.id || `input-${Math.random().toString(36).substr(2, 9)}`)

const handleInput = (event) => {
  emit('update:modelValue', event.target.value)
}

const handleBlur = (event) => {
  emit('blur', event.target.value)
}
</script>

<style scoped>
.form-input {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
  margin-bottom: 1rem;
}

.input-label {
  font-weight: 500;
  color: #2c3e50;
  font-size: 0.9rem;
}

.required-indicator {
  color: #e74c3c;
  margin-left: 0.25rem;
}

.input-field {
  padding: 0.75rem;
  border: 1px solid #bdc3c7;
  border-radius: 4px;
  font-size: 1rem;
  transition: border-color 0.2s, box-shadow 0.2s;
  background-color: white;
}

.input-field:focus {
  outline: none;
  border-color: #3498db;
  box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.1);
}

.input-field:disabled,
.input-disabled {
  background-color: #ecf0f1;
  cursor: not-allowed;
  opacity: 0.6;
}

.input-field:read-only {
  background-color: #f8f9fa;
}

.input-error {
  border-color: #e74c3c;
}

.input-error:focus {
  border-color: #e74c3c;
  box-shadow: 0 0 0 3px rgba(231, 76, 60, 0.1);
}

.error-text {
  margin: 0;
  color: #e74c3c;
  font-size: 0.85rem;
}

.hint-text {
  margin: 0;
  color: #7f8c8d;
  font-size: 0.85rem;
}
</style>
