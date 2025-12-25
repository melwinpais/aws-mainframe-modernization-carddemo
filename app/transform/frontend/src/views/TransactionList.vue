<template>
  <div class="transaction-list">
    
    <div class="container">
      <!-- Search Form -->
      <div class="search-section">
        <h2>Transaction Search</h2>
        
        <div class="search-form">
          <div class="form-row">
            <div class="form-group">
              <label for="searchType">Search By:</label>
              <select id="searchType" v-model="searchType" class="form-control">
                <option value="account">Account ID</option>
                <option value="card">Card Number</option>
              </select>
            </div>
            
            <div class="form-group">
              <label :for="searchType === 'account' ? 'accountId' : 'cardNumber'">
                {{ searchType === 'account' ? 'Account ID' : 'Card Number' }}:
              </label>
              <input
                :id="searchType === 'account' ? 'accountId' : 'cardNumber'"
                v-model="searchValue"
                type="text"
                class="form-control"
                :placeholder="searchType === 'account' ? 'Enter 11-digit account ID' : 'Enter 16-digit card number'"
                @keyup.enter="searchTransactions"
              />
            </div>
          </div>
          
          <div class="form-row">
            <div class="form-group">
              <label for="startDate">Start Date:</label>
              <input
                id="startDate"
                v-model="startDate"
                type="date"
                class="form-control"
              />
            </div>
            
            <div class="form-group">
              <label for="endDate">End Date:</label>
              <input
                id="endDate"
                v-model="endDate"
                type="date"
                class="form-control"
              />
            </div>
          </div>
          
          <div class="button-group">
            <button class="btn btn-primary" @click="searchTransactions" :disabled="loading">
              <span v-if="loading">Searching...</span>
              <span v-else>Search</span>
            </button>
            <button class="btn btn-secondary" @click="clearFilters">
              Clear Filters
            </button>
            <button class="btn btn-secondary" @click="returnToMenu">
              Return to Menu
            </button>
          </div>
        </div>
        
        <ErrorMessage v-if="errorMessage" :message="errorMessage" />
      </div>
      
      <!-- Transaction List -->
      <div v-if="transactions.length > 0" class="results-section">
        <h3>Transaction Results</h3>
        
        <div class="transaction-table-wrapper">
          <table class="transaction-table">
            <thead>
              <tr>
                <th>Transaction ID</th>
                <th>Date</th>
                <th>Time</th>
                <th>Type</th>
                <th>Category</th>
                <th>Amount</th>
                <th>Description</th>
                <th>Merchant</th>
                <th>Actions</th>
              </tr>
            </thead>
            <tbody>
              <tr v-for="transaction in transactions" :key="transaction.transactionId">
                <td>{{ transaction.transactionId }}</td>
                <td>{{ formatDate(transaction.transactionDate) }}</td>
                <td>{{ transaction.transactionTime || 'N/A' }}</td>
                <td>{{ transaction.transactionType }}</td>
                <td>{{ transaction.transactionCategory || 'N/A' }}</td>
                <td :class="getAmountClass(transaction.amount)">
                  {{ formatAmount(transaction.amount) }}
                </td>
                <td>{{ transaction.description || 'N/A' }}</td>
                <td>{{ transaction.merchantName || 'N/A' }}</td>
                <td>
                  <button 
                    class="btn btn-sm btn-info" 
                    @click="viewTransaction(transaction.transactionId)"
                  >
                    View Details
                  </button>
                </td>
              </tr>
            </tbody>
          </table>
        </div>
        
        <!-- Pagination -->
        <div v-if="totalPages > 1" class="pagination">
          <button 
            class="btn btn-sm btn-secondary" 
            @click="previousPage" 
            :disabled="currentPage === 0 || loading"
          >
            Previous
          </button>
          
          <span class="page-info">
            Page {{ currentPage + 1 }} of {{ totalPages }}
            ({{ totalElements }} total transactions)
          </span>
          
          <button 
            class="btn btn-sm btn-secondary" 
            @click="nextPage" 
            :disabled="currentPage >= totalPages - 1 || loading"
          >
            Next
          </button>
        </div>
      </div>
      
      <!-- No Results Message -->
      <div v-else-if="searchPerformed && !loading" class="no-results">
        <p>No transactions found for the specified criteria.</p>
      </div>
      
      <!-- Loading Spinner -->
      <LoadingSpinner v-if="loading" />
    </div>
  </div>
</template>

<script>
import { ref, computed } from 'vue'
import { useRouter } from 'vue-router'
import { useTransactionStore } from '@/stores/transaction'
import transactionService from '@/services/transactionService'
import ErrorMessage from '@/components/ErrorMessage.vue'
import LoadingSpinner from '@/components/LoadingSpinner.vue'

export default {
  name: 'TransactionList',
  components: {
    ErrorMessage,
    LoadingSpinner
  },
  setup() {
    const router = useRouter()
    const transactionStore = useTransactionStore()
    
    // Search form state
    const searchType = ref('account')
    const searchValue = ref('')
    const startDate = ref('')
    const endDate = ref('')
    const searchPerformed = ref(false)
    
    // UI state
    const loading = ref(false)
    const errorMessage = ref('')
    
    // Transaction data from store
    const transactions = computed(() => transactionStore.transactionList)
    const currentPage = computed(() => transactionStore.currentPage)
    const totalPages = computed(() => transactionStore.totalPages)
    const totalElements = computed(() => transactionStore.totalElements)
    const pageSize = computed(() => transactionStore.pageSize)
    
    /**
     * Validate search input
     */
    const validateInput = () => {
      if (!searchValue.value || searchValue.value.trim() === '') {
        errorMessage.value = `Please enter a ${searchType.value === 'account' ? 'account ID' : 'card number'}`
        return false
      }
      
      if (searchType.value === 'account') {
        if (!/^\d{11}$/.test(searchValue.value)) {
          errorMessage.value = 'Account ID must be exactly 11 digits'
          return false
        }
        if (searchValue.value === '00000000000') {
          errorMessage.value = 'Account ID cannot be all zeros'
          return false
        }
      } else {
        if (!/^\d{16}$/.test(searchValue.value)) {
          errorMessage.value = 'Card number must be exactly 16 digits'
          return false
        }
      }
      
      // Validate date range if provided
      if (startDate.value && endDate.value) {
        if (new Date(startDate.value) > new Date(endDate.value)) {
          errorMessage.value = 'Start date must be before or equal to end date'
          return false
        }
      }
      
      return true
    }
    
    /**
     * Search for transactions
     */
    const searchTransactions = async (page = 0) => {
      if (!validateInput()) {
        return
      }
      
      errorMessage.value = ''
      loading.value = true
      searchPerformed.value = true
      
      try {
        const params = {
          page,
          size: pageSize.value
        }
        
        if (startDate.value) {
          params.startDate = startDate.value
        }
        
        if (endDate.value) {
          params.endDate = endDate.value
        }
        
        let response
        if (searchType.value === 'account') {
          response = await transactionService.getTransactionsByAccount(searchValue.value, params)
        } else {
          response = await transactionService.getTransactionsByCard(searchValue.value, params)
        }
        
        // Update store with results
        transactionStore.setTransactionList(response.content || [])
        transactionStore.setPagination({
          currentPage: response.number || 0,
          totalPages: response.totalPages || 0,
          pageSize: response.size || 20,
          totalElements: response.totalElements || 0
        })
      } catch (error) {
        console.error('Error searching transactions:', error)
        if (error.response?.data?.message) {
          errorMessage.value = error.response.data.message
        } else if (error.response?.status === 404) {
          errorMessage.value = `No ${searchType.value === 'account' ? 'account' : 'card'} found with the specified ID`
        } else {
          errorMessage.value = 'Failed to retrieve transactions. Please try again.'
        }
        transactionStore.clearTransactionList()
      } finally {
        loading.value = false
      }
    }
    
    /**
     * Navigate to previous page
     */
    const previousPage = () => {
      if (currentPage.value > 0) {
        searchTransactions(currentPage.value - 1)
      }
    }
    
    /**
     * Navigate to next page
     */
    const nextPage = () => {
      if (currentPage.value < totalPages.value - 1) {
        searchTransactions(currentPage.value + 1)
      }
    }
    
    /**
     * Clear all filters and results
     */
    const clearFilters = () => {
      searchValue.value = ''
      startDate.value = ''
      endDate.value = ''
      errorMessage.value = ''
      searchPerformed.value = false
      transactionStore.clearTransactionList()
    }
    
    /**
     * View transaction details
     */
    const viewTransaction = (transactionId) => {
      router.push(`/transactions/view/${transactionId}`)
    }
    
    /**
     * Return to main menu
     */
    const returnToMenu = () => {
      router.push('/menu')
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
          month: '2-digit',
          day: '2-digit'
        })
      } catch {
        return dateStr
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
    
    return {
      searchType,
      searchValue,
      startDate,
      endDate,
      searchPerformed,
      loading,
      errorMessage,
      transactions,
      currentPage,
      totalPages,
      totalElements,
      searchTransactions,
      previousPage,
      nextPage,
      clearFilters,
      viewTransaction,
      returnToMenu,
      formatDate,
      formatAmount,
      getAmountClass
    }
  }
}
</script>

<style scoped>
.transaction-list {
  min-height: 100vh;
  background-color: #f5f5f5;
}

.container {
  max-width: 1400px;
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
  display: flex;
  flex-direction: column;
  gap: 1rem;
}

.form-row {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 1rem;
}

.form-group {
  display: flex;
  flex-direction: column;
}

.form-group label {
  margin-bottom: 0.5rem;
  font-weight: 500;
  color: #555;
}

.form-control {
  padding: 0.75rem;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 1rem;
}

.form-control:focus {
  outline: none;
  border-color: #007bff;
  box-shadow: 0 0 0 3px rgba(0, 123, 255, 0.1);
}

.button-group {
  display: flex;
  gap: 1rem;
  margin-top: 1rem;
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

.btn-sm {
  padding: 0.5rem 1rem;
  font-size: 0.875rem;
}

.btn-info {
  background-color: #17a2b8;
  color: white;
}

.btn-info:hover {
  background-color: #138496;
}

.results-section {
  background: white;
  padding: 2rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.results-section h3 {
  margin-top: 0;
  margin-bottom: 1.5rem;
  color: #333;
}

.transaction-table-wrapper {
  overflow-x: auto;
  margin-bottom: 1.5rem;
}

.transaction-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.9rem;
}

.transaction-table th,
.transaction-table td {
  padding: 0.75rem;
  text-align: left;
  border-bottom: 1px solid #ddd;
}

.transaction-table th {
  background-color: #f8f9fa;
  font-weight: 600;
  color: #333;
  white-space: nowrap;
}

.transaction-table tbody tr:hover {
  background-color: #f8f9fa;
}

.amount-positive {
  color: #28a745;
  font-weight: 600;
}

.amount-negative {
  color: #dc3545;
  font-weight: 600;
}

.pagination {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding-top: 1rem;
  border-top: 1px solid #ddd;
}

.page-info {
  color: #666;
  font-size: 0.9rem;
}

.no-results {
  background: white;
  padding: 3rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  text-align: center;
  color: #666;
}

@media (max-width: 768px) {
  .container {
    padding: 1rem;
  }
  
  .search-section,
  .results-section {
    padding: 1rem;
  }
  
  .form-row {
    grid-template-columns: 1fr;
  }
  
  .button-group {
    flex-direction: column;
  }
  
  .btn {
    width: 100%;
  }
  
  .transaction-table {
    font-size: 0.8rem;
  }
  
  .transaction-table th,
  .transaction-table td {
    padding: 0.5rem;
  }
  
  .pagination {
    flex-direction: column;
    gap: 1rem;
  }
}
</style>
