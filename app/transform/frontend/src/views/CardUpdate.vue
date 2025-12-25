<template>
  <div class="card-update">
    
    <div class="container">
      <!-- Loading Spinner -->
      <LoadingSpinner v-if="loading" message="Loading card details..." />

      <!-- Error Message -->
      <ErrorMessage v-if="errorMessage" :message="errorMessage" @close="clearError" />

      <!-- Success Message -->
      <div v-if="successMessage" class="alert alert-success">
        {{ successMessage }}
      </div>

      <!-- Card Update Form -->
      <div v-if="!loading && card" class="update-form-section">
        <h2>Update Card Information</h2>
        
        <form @submit.prevent="saveChanges" class="update-form">
          <!-- Card Number (Read-only) -->
          <div class="form-group">
            <label>Card Number:</label>
            <div class="readonly-value">{{ maskCardNumber(card.cardNumber) }}</div>
          </div>

          <!-- Account ID (Read-only) -->
          <div class="form-group">
            <label>Account ID:</label>
            <div class="readonly-value">{{ card.accountId }}</div>
          </div>

          <!-- Customer ID (Read-only) -->
          <div class="form-group">
            <label>Customer ID:</label>
            <div class="readonly-value">{{ card.customerId }}</div>
          </div>

          <!-- Card Status -->
          <FormInput
            v-model="card.cardStatus"
            label="Card Status"
            type="text"
            :error="fieldErrors.cardStatus"
            maxlength="10"
            required
          />

          <!-- Expiration Date -->
          <FormInput
            v-model="card.expirationDate"
            label="Expiration Date"
            type="text"
            placeholder="YYYY-MM-DD"
            :error="fieldErrors.expirationDate"
            maxlength="10"
          />

          <!-- Issue Date -->
          <FormInput
            v-model="card.issueDate"
            label="Issue Date"
            type="text"
            placeholder="YYYY-MM-DD"
            :error="fieldErrors.issueDate"
            maxlength="10"
          />

          <!-- Change Indicator -->
          <div v-if="hasChanges" class="changes-indicator">
            <span class="changes-badge">Unsaved Changes</span>
          </div>

          <!-- Action Buttons -->
          <div class="button-group">
            <button 
              type="submit" 
              class="btn btn-primary" 
              :disabled="!hasChanges || saving"
            >
              <span v-if="saving">Saving...</span>
              <span v-else>Save Changes</span>
            </button>
            <button 
              type="button" 
              class="btn btn-secondary" 
              @click="cancelChanges"
              :disabled="saving"
            >
              Cancel
            </button>
          </div>
        </form>
      </div>

      <!-- Card Not Found -->
      <div v-if="!loading && !card && !errorMessage" class="not-found">
        <p>Card not found</p>
        <button class="btn btn-secondary" @click="returnToList">
          Return to Card List
        </button>
      </div>
    </div>
  </div>
</template>

<script>
import { ref, computed, watch, onMounted } from 'vue'
import { useRouter, useRoute } from 'vue-router'
import { useCardStore } from '../stores/card'
import FormInput from '../components/FormInput.vue'
import ErrorMessage from '../components/ErrorMessage.vue'
import LoadingSpinner from '../components/LoadingSpinner.vue'
import cardService from '../services/cardService'

export default {
  name: 'CardUpdate',
  components: {
    FormInput,
    ErrorMessage,
    LoadingSpinner
  },
  setup() {
    const router = useRouter()
    const route = useRoute()
    const cardStore = useCardStore()

    const card = ref(null)
    const originalCard = ref(null)
    const errorMessage = ref('')
    const successMessage = ref('')
    const fieldErrors = ref({})
    const loading = ref(false)
    const saving = ref(false)

    /**
     * Check if there are unsaved changes
     */
    const hasChanges = computed(() => {
      if (!card.value || !originalCard.value) return false
      
      return (
        card.value.cardStatus !== originalCard.value.cardStatus ||
        card.value.expirationDate !== originalCard.value.expirationDate ||
        card.value.issueDate !== originalCard.value.issueDate
      )
    })

    /**
     * Load card details
     */
    const loadCard = async () => {
      const cardNumber = route.params.cardNumber
      
      if (!cardNumber) {
        errorMessage.value = 'Card number not provided'
        return
      }

      loading.value = true
      errorMessage.value = ''

      try {
        const cardData = await cardService.getCard(cardNumber)
        card.value = { ...cardData }
        originalCard.value = { ...cardData }
        cardStore.setCard(cardData)
      } catch (error) {
        console.error('Error loading card:', error)
        if (error.response?.data?.message) {
          errorMessage.value = error.response.data.message
        } else if (error.response?.status === 404) {
          errorMessage.value = 'Card not found'
        } else {
          errorMessage.value = 'Error loading card details. Please try again.'
        }
      } finally {
        loading.value = false
      }
    }

    /**
     * Validate all fields
     */
    const validateFields = () => {
      fieldErrors.value = {}
      let isValid = true

      // Validate card status
      if (!card.value.cardStatus || card.value.cardStatus.trim() === '') {
        fieldErrors.value.cardStatus = 'Card status is required'
        isValid = false
      }

      // Validate expiration date format if provided
      if (card.value.expirationDate && card.value.expirationDate.trim() !== '') {
        const datePattern = /^\d{4}-\d{2}-\d{2}$/
        if (!datePattern.test(card.value.expirationDate)) {
          fieldErrors.value.expirationDate = 'Date must be in YYYY-MM-DD format'
          isValid = false
        }
      }

      // Validate issue date format if provided
      if (card.value.issueDate && card.value.issueDate.trim() !== '') {
        const datePattern = /^\d{4}-\d{2}-\d{2}$/
        if (!datePattern.test(card.value.issueDate)) {
          fieldErrors.value.issueDate = 'Date must be in YYYY-MM-DD format'
          isValid = false
        }
      }

      return isValid
    }

    /**
     * Save changes
     */
    const saveChanges = async () => {
      if (!validateFields()) {
        errorMessage.value = 'Please correct the errors before saving'
        return
      }

      saving.value = true
      errorMessage.value = ''
      successMessage.value = ''

      try {
        // Prepare update data (only changed fields)
        const updates = {}
        if (card.value.cardStatus !== originalCard.value.cardStatus) {
          updates.cardStatus = card.value.cardStatus
        }
        if (card.value.expirationDate !== originalCard.value.expirationDate) {
          updates.expirationDate = card.value.expirationDate
        }
        if (card.value.issueDate !== originalCard.value.issueDate) {
          updates.issueDate = card.value.issueDate
        }

        const updatedCard = await cardService.updateCard(card.value.cardNumber, updates)
        
        // Update local state
        card.value = { ...updatedCard }
        originalCard.value = { ...updatedCard }
        cardStore.setCard(updatedCard)
        cardStore.updateCardInList(updatedCard)

        successMessage.value = 'Card updated successfully'
        
        // Navigate back to card detail after a short delay
        setTimeout(() => {
          router.push(`/cards/detail/${card.value.cardNumber}`)
        }, 1500)
      } catch (error) {
        console.error('Error updating card:', error)
        
        // Handle field-specific errors
        if (error.response?.data?.fieldErrors) {
          error.response.data.fieldErrors.forEach(fieldError => {
            fieldErrors.value[fieldError.field] = fieldError.message
          })
          errorMessage.value = 'Please correct the errors below'
        } else if (error.response?.data?.message) {
          errorMessage.value = error.response.data.message
        } else {
          errorMessage.value = 'Error updating card. Please try again.'
        }
      } finally {
        saving.value = false
      }
    }

    /**
     * Cancel changes and return to previous view
     */
    const cancelChanges = () => {
      if (hasChanges.value) {
        const confirmed = confirm('You have unsaved changes. Are you sure you want to cancel?')
        if (!confirmed) return
      }
      router.push(`/cards/detail/${route.params.cardNumber}`)
    }

    /**
     * Return to card list
     */
    const returnToList = () => {
      router.push('/cards/list')
    }

    /**
     * Mask card number (show only last 4 digits)
     */
    const maskCardNumber = (cardNumber) => {
      if (!cardNumber) return ''
      const cardStr = String(cardNumber)
      if (cardStr.length !== 16) return cardStr
      return '**** **** **** ' + cardStr.slice(-4)
    }

    /**
     * Clear error message
     */
    const clearError = () => {
      errorMessage.value = ''
    }

    // Load card on mount
    onMounted(() => {
      loadCard()
    })

    // Watch for navigation away with unsaved changes
    watch(
      () => route.path,
      (newPath, oldPath) => {
        if (oldPath && newPath !== oldPath && hasChanges.value) {
          const confirmed = confirm('You have unsaved changes. Are you sure you want to leave?')
          if (!confirmed) {
            router.push(oldPath)
          }
        }
      }
    )

    return {
      card,
      errorMessage,
      successMessage,
      fieldErrors,
      loading,
      saving,
      hasChanges,
      saveChanges,
      cancelChanges,
      returnToList,
      maskCardNumber,
      clearError
    }
  }
}
</script>

<style scoped>
.card-update {
  min-height: 100vh;
  background-color: #f5f5f5;
}

.container {
  max-width: 800px;
  margin: 0 auto;
  padding: 2rem;
}

.update-form-section {
  background: white;
  padding: 2rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.update-form-section h2 {
  margin-top: 0;
  margin-bottom: 1.5rem;
  color: #333;
}

.update-form {
  max-width: 600px;
}

.form-group {
  margin-bottom: 1.5rem;
}

.form-group label {
  display: block;
  font-weight: 600;
  color: #495057;
  margin-bottom: 0.5rem;
  font-size: 0.9rem;
}

.readonly-value {
  color: #6c757d;
  font-size: 1rem;
  padding: 0.5rem;
  background-color: #e9ecef;
  border-radius: 4px;
  font-style: italic;
}

.alert {
  padding: 1rem;
  border-radius: 4px;
  margin-bottom: 1.5rem;
}

.alert-success {
  background-color: #d4edda;
  color: #155724;
  border: 1px solid #c3e6cb;
}

.changes-indicator {
  margin-bottom: 1.5rem;
}

.changes-badge {
  display: inline-block;
  padding: 0.5rem 1rem;
  background-color: #fff3cd;
  color: #856404;
  border-radius: 4px;
  font-weight: 500;
  font-size: 0.9rem;
}

.button-group {
  display: flex;
  gap: 1rem;
  margin-top: 2rem;
}

.btn {
  padding: 0.75rem 1.5rem;
  border: none;
  border-radius: 4px;
  font-size: 1rem;
  cursor: pointer;
  transition: background-color 0.2s;
}

.btn:disabled {
  opacity: 0.6;
  cursor: not-allowed;
}

.btn-primary {
  background-color: #007bff;
  color: white;
}

.btn-primary:hover:not(:disabled) {
  background-color: #0056b3;
}

.btn-secondary {
  background-color: #6c757d;
  color: white;
}

.btn-secondary:hover:not(:disabled) {
  background-color: #545b62;
}

.not-found {
  background: white;
  padding: 2rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  text-align: center;
}

.not-found p {
  color: #6c757d;
  font-size: 1.1rem;
  margin-bottom: 1.5rem;
}

@media (max-width: 768px) {
  .container {
    padding: 1rem;
  }

  .update-form-section {
    padding: 1rem;
  }

  .button-group {
    flex-direction: column;
  }

  .btn {
    width: 100%;
  }
}
</style>
