<template>
  <div class="card-detail">
    <div class="container">
      <!-- Loading Spinner -->
      <LoadingSpinner v-if="loading" message="Loading card details..." />

      <!-- Error Message -->
      <ErrorMessage v-if="errorMessage" :message="errorMessage" @close="clearError" />

      <!-- Card Details -->
      <div v-if="!loading && card" class="card-details-section">
        <h2>Card Information</h2>
        
        <div class="details-grid">
          <!-- Card Number -->
          <div class="detail-item">
            <label>Card Number:</label>
            <div class="detail-value">{{ maskCardNumber(card.cardNumber) }}</div>
          </div>

          <!-- Account ID -->
          <div class="detail-item">
            <label>Account ID:</label>
            <div class="detail-value">{{ card.accountId }}</div>
          </div>

          <!-- Customer ID -->
          <div class="detail-item">
            <label>Customer ID:</label>
            <div class="detail-value">{{ card.customerId }}</div>
          </div>

          <!-- Card Status -->
          <div class="detail-item">
            <label>Status:</label>
            <div class="detail-value">
              <span :class="['status-badge', getStatusClass(card.cardStatus)]">
                {{ card.cardStatus }}
              </span>
            </div>
          </div>

          <!-- Expiration Date -->
          <div class="detail-item">
            <label>Expiration Date:</label>
            <div class="detail-value">{{ formatDate(card.expirationDate) }}</div>
          </div>

          <!-- Issue Date -->
          <div class="detail-item">
            <label>Issue Date:</label>
            <div class="detail-value">{{ formatDate(card.issueDate) }}</div>
          </div>
        </div>

        <!-- Action Buttons -->
        <div class="button-group">
          <button class="btn btn-primary" @click="navigateToUpdate">
            Update Card
          </button>
          <button class="btn btn-secondary" @click="returnToList">
            Return to Card List
          </button>
        </div>
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
import { ref, onMounted } from 'vue'
import { useRouter, useRoute } from 'vue-router'
import { useCardStore } from '../stores/card'
import ErrorMessage from '../components/ErrorMessage.vue'
import LoadingSpinner from '../components/LoadingSpinner.vue'
import cardService from '../services/cardService'

export default {
  name: 'CardDetail',
  components: {
    ErrorMessage,
    LoadingSpinner
  },
  setup() {
    const router = useRouter()
    const route = useRoute()
    const cardStore = useCardStore()

    const card = ref(null)
    const errorMessage = ref('')
    const loading = ref(false)

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
        card.value = cardData
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
     * Mask card number (show only last 4 digits)
     */
    const maskCardNumber = (cardNumber) => {
      if (!cardNumber) return ''
      const cardStr = String(cardNumber)
      if (cardStr.length !== 16) return cardStr
      return '**** **** **** ' + cardStr.slice(-4)
    }

    /**
     * Format date for display
     */
    const formatDate = (date) => {
      if (!date) return 'N/A'
      return date
    }

    /**
     * Get CSS class for card status
     */
    const getStatusClass = (status) => {
      if (!status) return ''
      const statusUpper = status.toUpperCase()
      if (statusUpper.includes('ACTIVE')) return 'status-active'
      if (statusUpper.includes('INACTIVE') || statusUpper.includes('CLOSED')) return 'status-inactive'
      if (statusUpper.includes('BLOCKED') || statusUpper.includes('SUSPENDED')) return 'status-blocked'
      return ''
    }

    /**
     * Navigate to card update view
     */
    const navigateToUpdate = () => {
      router.push(`/cards/update/${route.params.cardNumber}`)
    }

    /**
     * Return to card list
     */
    const returnToList = () => {
      router.push('/cards/list')
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

    return {
      card,
      errorMessage,
      loading,
      maskCardNumber,
      formatDate,
      getStatusClass,
      navigateToUpdate,
      returnToList,
      clearError
    }
  }
}
</script>

<style scoped>
.card-detail {
  min-height: 100vh;
  background-color: #f5f5f5;
}

.container {
  max-width: 800px;
  margin: 0 auto;
  padding: 2rem;
}

.card-details-section {
  background: white;
  padding: 2rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.card-details-section h2 {
  margin-top: 0;
  margin-bottom: 1.5rem;
  color: #333;
}

.details-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 1.5rem;
  margin-bottom: 2rem;
}

.detail-item {
  display: flex;
  flex-direction: column;
}

.detail-item label {
  font-weight: 600;
  color: #495057;
  margin-bottom: 0.5rem;
  font-size: 0.9rem;
}

.detail-value {
  color: #212529;
  font-size: 1rem;
  padding: 0.5rem;
  background-color: #f8f9fa;
  border-radius: 4px;
}

.status-badge {
  display: inline-block;
  padding: 0.25rem 0.75rem;
  border-radius: 12px;
  font-size: 0.875rem;
  font-weight: 500;
}

.status-active {
  background-color: #d4edda;
  color: #155724;
}

.status-inactive {
  background-color: #f8d7da;
  color: #721c24;
}

.status-blocked {
  background-color: #fff3cd;
  color: #856404;
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

.btn-primary {
  background-color: #007bff;
  color: white;
}

.btn-primary:hover {
  background-color: #0056b3;
}

.btn-secondary {
  background-color: #6c757d;
  color: white;
}

.btn-secondary:hover {
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

  .card-details-section {
    padding: 1rem;
  }

  .details-grid {
    grid-template-columns: 1fr;
    gap: 1rem;
  }

  .button-group {
    flex-direction: column;
  }

  .btn {
    width: 100%;
  }
}
</style>
