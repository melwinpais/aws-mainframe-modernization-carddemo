<template>
  <div class="card-list">
    
    <div class="container">
      <!-- Search Section -->
      <div class="search-section">
        <h2>Search Cards by Account</h2>
        <form @submit.prevent="searchCards" class="search-form">
          <FormInput
            v-model="accountId"
            label="Account ID"
            type="text"
            placeholder="Enter 11-digit account ID"
            :error="accountIdError"
            maxlength="11"
            required
          />
          
          <div class="button-group">
            <button type="submit" class="btn btn-primary" :disabled="loading">
              <span v-if="loading">Searching...</span>
              <span v-else>Search Cards</span>
            </button>
            <button type="button" class="btn btn-secondary" @click="returnToMenu">
              Return to Menu
            </button>
          </div>
        </form>
      </div>

      <!-- Error Message -->
      <ErrorMessage v-if="errorMessage" :message="errorMessage" @close="clearError" />

      <!-- Loading Spinner -->
      <LoadingSpinner v-if="loading" message="Loading cards..." />

      <!-- Cards List -->
      <div v-if="!loading && cards.length > 0" class="cards-section">
        <h3>Cards for Account {{ accountId }}</h3>
        <div class="cards-table-container">
          <table class="cards-table">
            <thead>
              <tr>
                <th>Card Number</th>
                <th>Customer ID</th>
                <th>Status</th>
                <th>Expiration Date</th>
                <th>Issue Date</th>
                <th>Actions</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="card in cards" :key="card.cardNumber">
                <td>{{ maskCardNumber(card.cardNumber) }}</td>
                <td>{{ card.customerId }}</td>
                <td>
                  <span :class="['status-badge', getStatusClass(card.cardStatus)]">
                    {{ card.cardStatus }}
                  </span>
                </td>
                <td>{{ formatDate(card.expirationDate) }}</td>
                <td>{{ formatDate(card.issueDate) }}</td>
                <td class="actions">
                  <button 
                    class="btn btn-sm btn-info" 
                    @click="viewCardDetails(card.cardNumber)"
                    title="View Details"
                  >
                    View
                  </button>
                  <button 
                    class="btn btn-sm btn-warning" 
                    @click="updateCard(card.cardNumber)"
                    title="Update Card"
                  >
                    Update
                  </button>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>

      <!-- No Results Message -->
      <div v-if="!loading && searchPerformed && cards.length === 0" class="no-results">
        <p>No cards found for account {{ accountId }}</p>
      </div>
    </div>
  </div>
</template>

<script>
import { ref, computed } from 'vue'
import { useRouter } from 'vue-router'
import { useCardStore } from '../stores/card'
import FormInput from '../components/FormInput.vue'
import ErrorMessage from '../components/ErrorMessage.vue'
import LoadingSpinner from '../components/LoadingSpinner.vue'
import cardService from '../services/cardService'

export default {
  name: 'CardList',
  components: {
    FormInput,
    ErrorMessage,
    LoadingSpinner
  },
  setup() {
    const router = useRouter()
    const cardStore = useCardStore()

    const accountId = ref('')
    const accountIdError = ref('')
    const errorMessage = ref('')
    const loading = ref(false)
    const searchPerformed = ref(false)

    const cards = computed(() => cardStore.cardList)

    /**
     * Validate account ID format
     */
    const validateAccountId = () => {
      accountIdError.value = ''
      
      if (!accountId.value || accountId.value.trim() === '') {
        accountIdError.value = 'Account number not provided'
        return false
      }

      if (accountId.value === '00000000000') {
        accountIdError.value = 'Account number must be a non zero 11 digit number'
        return false
      }

      if (!/^\d{11}$/.test(accountId.value)) {
        accountIdError.value = 'Account number must be a non zero 11 digit number'
        return false
      }

      return true
    }

    /**
     * Search for cards by account ID
     */
    const searchCards = async () => {
      if (!validateAccountId()) {
        return
      }

      loading.value = true
      errorMessage.value = ''
      cardStore.clearCardList()
      searchPerformed.value = false

      try {
        const cardList = await cardService.getCardsByAccount(accountId.value)
        cardStore.setCardList(cardList)
        searchPerformed.value = true
      } catch (error) {
        console.error('Error loading cards:', error)
        if (error.response?.data?.message) {
          errorMessage.value = error.response.data.message
        } else if (error.response?.status === 404) {
          errorMessage.value = 'No cards found for this account'
        } else {
          errorMessage.value = 'Error loading cards. Please try again.'
        }
        searchPerformed.value = true
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
     * Navigate to card detail view
     */
    const viewCardDetails = (cardNumber) => {
      router.push(`/cards/detail/${cardNumber}`)
    }

    /**
     * Navigate to card update view
     */
    const updateCard = (cardNumber) => {
      router.push(`/cards/update/${cardNumber}`)
    }

    /**
     * Return to main menu
     */
    const returnToMenu = () => {
      router.push('/menu')
    }

    /**
     * Clear error message
     */
    const clearError = () => {
      errorMessage.value = ''
    }

    return {
      accountId,
      accountIdError,
      errorMessage,
      loading,
      searchPerformed,
      cards,
      searchCards,
      maskCardNumber,
      formatDate,
      getStatusClass,
      viewCardDetails,
      updateCard,
      returnToMenu,
      clearError
    }
  }
}
</script>

<style scoped>
.card-list {
  min-height: 100vh;
  background-color: #f5f5f5;
}

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 2rem;
}

.search-section {
  background: white;
  padding: 2rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  margin-bottom: 2rem;
}

.search-section h2 {
  margin-top: 0;
  margin-bottom: 1.5rem;
  color: #333;
}

.search-form {
  max-width: 500px;
}

.button-group {
  display: flex;
  gap: 1rem;
  margin-top: 1.5rem;
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

.btn-secondary:hover {
  background-color: #545b62;
}

.btn-sm {
  padding: 0.375rem 0.75rem;
  font-size: 0.875rem;
}

.btn-info {
  background-color: #17a2b8;
  color: white;
}

.btn-info:hover {
  background-color: #117a8b;
}

.btn-warning {
  background-color: #ffc107;
  color: #212529;
}

.btn-warning:hover {
  background-color: #e0a800;
}

.cards-section {
  background: white;
  padding: 2rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.cards-section h3 {
  margin-top: 0;
  margin-bottom: 1.5rem;
  color: #333;
}

.cards-table-container {
  overflow-x: auto;
}

.cards-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.95rem;
}

.cards-table th,
.cards-table td {
  padding: 0.75rem;
  text-align: left;
  border-bottom: 1px solid #dee2e6;
}

.cards-table th {
  background-color: #f8f9fa;
  font-weight: 600;
  color: #495057;
}

.cards-table tbody tr:hover {
  background-color: #f8f9fa;
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

.actions {
  display: flex;
  gap: 0.5rem;
}

.no-results {
  background: white;
  padding: 2rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  text-align: center;
  color: #6c757d;
}

@media (max-width: 768px) {
  .container {
    padding: 1rem;
  }

  .search-section,
  .cards-section {
    padding: 1rem;
  }

  .button-group {
    flex-direction: column;
  }

  .btn {
    width: 100%;
  }

  .cards-table {
    font-size: 0.85rem;
  }

  .cards-table th,
  .cards-table td {
    padding: 0.5rem;
  }

  .actions {
    flex-direction: column;
  }
}
</style>
