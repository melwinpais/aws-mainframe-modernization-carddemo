<template>
  <div class="transaction-view">
    
    <div class="container">
      <LoadingSpinner v-if="loading" />
      
      <ErrorMessage v-if="errorMessage" :message="errorMessage" />
      
      <div v-if="transaction && !loading" class="transaction-details">
        <h2>Transaction Information</h2>
        
        <div class="details-grid">
          <!-- Transaction Identification -->
          <div class="detail-section">
            <h3>Transaction Identification</h3>
            <div class="detail-row">
              <span class="detail-label">Transaction ID:</span>
              <span class="detail-value">{{ transaction.transactionId }}</span>
            </div>
            <div class="detail-row">
              <span class="detail-label">Account ID:</span>
              <span class="detail-value">{{ transaction.accountId }}</span>
            </div>
            <div class="detail-row">
              <span class="detail-label">Card Number:</span>
              <span class="detail-value">{{ formatCardNumber(transaction.cardNumber) }}</span>
            </div>
          </div>
          
          <!-- Transaction Details -->
          <div class="detail-section">
            <h3>Transaction Details</h3>
            <div class="detail-row">
              <span class="detail-label">Type:</span>
              <span class="detail-value">{{ transaction.transactionType }}</span>
            </div>
            <div class="detail-row">
              <span class="detail-label">Category:</span>
              <span class="detail-value">{{ transaction.transactionCategory || 'N/A' }}</span>
            </div>
            <div class="detail-row">
              <span class="detail-label">Amount:</span>
              <span class="detail-value" :class="getAmountClass(transaction.amount)">
                {{ formatAmount(transaction.amount) }}
              </span>
            </div>
            <div class="detail-row">
              <span class="detail-label">Description:</span>
              <span class="detail-value">{{ transaction.description || 'N/A' }}</span>
            </div>
          </div>
          
          <!-- Date and Time -->
          <div class="detail-section">
            <h3>Date and Time</h3>
            <div class="detail-row">
              <span class="detail-label">Transaction Date:</span>
              <span class="detail-value">{{ formatDate(transaction.transactionDate) }}</span>
            </div>
            <div class="detail-row">
              <span class="detail-label">Transaction Time:</span>
              <span class="detail-value">{{ transaction.transactionTime || 'N/A' }}</span>
            </div>
            <div class="detail-row">
              <span class="detail-label">Created At:</span>
              <span class="detail-value">{{ formatDateTime(transaction.createdAt) }}</span>
            </div>
          </div>
          
          <!-- Merchant Information -->
          <div class="detail-section">
            <h3>Merchant Information</h3>
            <div class="detail-row">
              <span class="detail-label">Merchant Name:</span>
              <span class="detail-value">{{ transaction.merchantName || 'N/A' }}</span>
            </div>
            <div class="detail-row">
              <span class="detail-label">Merchant City:</span>
              <span class="detail-value">{{ transaction.merchantCity || 'N/A' }}</span>
            </div>
            <div class="detail-row">
              <span class="detail-label">Merchant Zip:</span>
              <span class="detail-value">{{ transaction.merchantZip || 'N/A' }}</span>
            </div>
          </div>
        </div>
        
        <!-- Action Buttons -->
        <div class="button-group">
          <button class="btn btn-secondary" @click="returnToList">
            Return to Transaction List
          </button>
          <button class="btn btn-secondary" @click="returnToMenu">
            Return to Menu
          </button>
        </div>
      </div>
      
      <!-- No Transaction Found -->
      <div v-if="!transaction && !loading && !errorMessage" class="no-transaction">
        <p>Transaction not found.</p>
        <button class="btn btn-secondary" @click="returnToList">
          Return to Transaction List
        </button>
      </div>
    </div>
  </div>
</template>

<script>
import { ref, onMounted } from 'vue'
import { useRoute, useRouter } from 'vue-router'
import { useTransactionStore } from '@/stores/transaction'
import transactionService from '@/services/transactionService'
import ErrorMessage from '@/components/ErrorMessage.vue'
import LoadingSpinner from '@/components/LoadingSpinner.vue'

export default {
  name: 'TransactionView',
  components: {
    ErrorMessage,
    LoadingSpinner
  },
  setup() {
    const route = useRoute()
    const router = useRouter()
    const transactionStore = useTransactionStore()
    
    const transaction = ref(null)
    const loading = ref(false)
    const errorMessage = ref('')
    
    /**
     * Load transaction details
     */
    const loadTransaction = async () => {
      const transactionId = route.params.transactionId
      
      if (!transactionId) {
        errorMessage.value = 'Transaction ID is required'
        return
      }
      
      loading.value = true
      errorMessage.value = ''
      
      try {
        const data = await transactionService.getTransaction(transactionId)
        transaction.value = data
        transactionStore.setTransaction(data)
      } catch (error) {
        console.error('Error loading transaction:', error)
        if (error.response?.data?.message) {
          errorMessage.value = error.response.data.message
        } else if (error.response?.status === 404) {
          errorMessage.value = 'Transaction not found'
        } else {
          errorMessage.value = 'Failed to load transaction details. Please try again.'
        }
      } finally {
        loading.value = false
      }
    }
    
    /**
     * Format card number (mask all but last 4 digits)
     */
    const formatCardNumber = (cardNumber) => {
      if (!cardNumber) return 'N/A'
      const cardStr = String(cardNumber)
      if (cardStr.length === 16) {
        return `**** **** **** ${cardStr.slice(-4)}`
      }
      return cardStr
    }
    
    /**
     * Format date for display
     */
    const formatDate = (dateStr) => {
      if (!dateStr) return 'N/A'
      try {
        const date = new Date(dateStr)
        return date.toLocaleDateString('en-US', {
          year: 'numeric',
          month: 'long',
          day: 'numeric'
        })
      } catch {
        return dateStr
      }
    }
    
    /**
     * Format date and time for display
     */
    const formatDateTime = (dateTimeStr) => {
      if (!dateTimeStr) return 'N/A'
      try {
        const date = new Date(dateTimeStr)
        return date.toLocaleString('en-US', {
          year: 'numeric',
          month: 'long',
          day: 'numeric',
          hour: '2-digit',
          minute: '2-digit',
          second: '2-digit'
        })
      } catch {
        return dateTimeStr
      }
    }
    
    /**
     * Format amount for display
     */
    const formatAmount = (amount) => {
      if (amount === null || amount === undefined) return 'N/A'
      const num = parseFloat(amount)
      return new Intl.NumberFormat('en-US', {
        style: 'currency',
        currency: 'USD'
      }).format(num)
    }
    
    /**
     * Get CSS class for amount based on positive/negative
     */
    const getAmountClass = (amount) => {
      const num = parseFloat(amount)
      if (num < 0) return 'amount-negative'
      if (num > 0) return 'amount-positive'
      return ''
    }
    
    /**
     * Return to transaction list
     */
    const returnToList = () => {
      router.push('/transactions/list')
    }
    
    /**
     * Return to main menu
     */
    const returnToMenu = () => {
      router.push('/menu')
    }
    
    // Load transaction on component mount
    onMounted(() => {
      loadTransaction()
    })
    
    return {
      transaction,
      loading,
      errorMessage,
      formatCardNumber,
      formatDate,
      formatDateTime,
      formatAmount,
      getAmountClass,
      returnToList,
      returnToMenu
    }
  }
}
</script>

<style scoped>
.transaction-view {
  min-height: 100vh;
  background-color: #f5f5f5;
}

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 2rem;
}

.transaction-details {
  background: white;
  padding: 2rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.transaction-details h2 {
  margin-top: 0;
  margin-bottom: 2rem;
  color: #333;
  border-bottom: 2px solid #007bff;
  padding-bottom: 0.5rem;
}

.details-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 2rem;
  margin-bottom: 2rem;
}

.detail-section {
  background: #f8f9fa;
  padding: 1.5rem;
  border-radius: 6px;
  border-left: 4px solid #007bff;
}

.detail-section h3 {
  margin-top: 0;
  margin-bottom: 1rem;
  color: #333;
  font-size: 1.1rem;
}

.detail-row {
  display: flex;
  justify-content: space-between;
  padding: 0.75rem 0;
  border-bottom: 1px solid #dee2e6;
}

.detail-row:last-child {
  border-bottom: none;
}

.detail-label {
  font-weight: 600;
  color: #555;
  flex: 0 0 45%;
}

.detail-value {
  color: #333;
  flex: 0 0 55%;
  text-align: right;
  word-break: break-word;
}

.amount-positive {
  color: #28a745;
  font-weight: 700;
  font-size: 1.1rem;
}

.amount-negative {
  color: #dc3545;
  font-weight: 700;
  font-size: 1.1rem;
}

.button-group {
  display: flex;
  gap: 1rem;
  padding-top: 1.5rem;
  border-top: 1px solid #dee2e6;
}

.btn {
  padding: 0.75rem 1.5rem;
  border: none;
  border-radius: 4px;
  font-size: 1rem;
  cursor: pointer;
  transition: background-color 0.2s;
}

.btn-secondary {
  background-color: #6c757d;
  color: white;
}

.btn-secondary:hover {
  background-color: #545b62;
}

.no-transaction {
  background: white;
  padding: 3rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  text-align: center;
}

.no-transaction p {
  color: #666;
  font-size: 1.1rem;
  margin-bottom: 1.5rem;
}

@media (max-width: 768px) {
  .container {
    padding: 1rem;
  }
  
  .transaction-details {
    padding: 1rem;
  }
  
  .details-grid {
    grid-template-columns: 1fr;
    gap: 1rem;
  }
  
  .detail-section {
    padding: 1rem;
  }
  
  .detail-row {
    flex-direction: column;
    gap: 0.5rem;
  }
  
  .detail-label,
  .detail-value {
    flex: 1;
    text-align: left;
  }
  
  .button-group {
    flex-direction: column;
  }
  
  .btn {
    width: 100%;
  }
}
</style>
