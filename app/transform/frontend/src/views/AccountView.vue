<template>
  <div class="account-view">
    
    <div class="container">
      <!-- Account Search Form -->
      <div class="search-section">
        <h2>Search Account</h2>
        <form @submit.prevent="searchAccount" class="search-form">
          <FormInput
            v-model="accountId"
            label="Account ID"
            type="text"
            placeholder="Enter 11-digit account ID"
            :error="validationError"
            :disabled="loading"
            maxlength="11"
            @input="clearError"
          />
          
          <div class="button-group">
            <button type="submit" class="btn btn-primary" :disabled="loading">
              <LoadingSpinner v-if="loading" size="small" />
              <span v-else>Search</span>
            </button>
            <button type="button" class="btn btn-secondary" @click="returnToMenu" :disabled="loading">
              Return to Menu
            </button>
          </div>
        </form>
        
        <ErrorMessage v-if="errorMessage" :message="errorMessage" @close="clearError" />
      </div>

      <!-- Account Details Display -->
      <div v-if="account" class="account-details">
        <div class="section">
          <h2>Account Information</h2>
          <div class="details-grid">
            <div class="detail-item">
              <label>Account ID:</label>
              <span>{{ account.accountId }}</span>
            </div>
            <div class="detail-item">
              <label>Active Status:</label>
              <span :class="{'status-active': account.activeStatus === 'Y', 'status-inactive': account.activeStatus === 'N'}">
                {{ account.activeStatus === 'Y' ? 'Active' : 'Inactive' }}
              </span>
            </div>
            <div class="detail-item">
              <label>Current Balance:</label>
              <span class="currency">{{ formatCurrency(account.currentBalance) }}</span>
            </div>
            <div class="detail-item">
              <label>Credit Limit:</label>
              <span class="currency">{{ formatCurrency(account.creditLimit) }}</span>
            </div>
            <div class="detail-item">
              <label>Cash Credit Limit:</label>
              <span class="currency">{{ formatCurrency(account.cashCreditLimit) }}</span>
            </div>
            <div class="detail-item">
              <label>Open Date:</label>
              <span>{{ account.openDate }}</span>
            </div>
            <div class="detail-item">
              <label>Expiration Date:</label>
              <span>{{ account.expirationDate || 'N/A' }}</span>
            </div>
            <div class="detail-item">
              <label>Reissue Date:</label>
              <span>{{ account.reissueDate || 'N/A' }}</span>
            </div>
            <div class="detail-item">
              <label>Current Cycle Credit:</label>
              <span class="currency">{{ formatCurrency(account.currentCycleCredit) }}</span>
            </div>
            <div class="detail-item">
              <label>Current Cycle Debit:</label>
              <span class="currency">{{ formatCurrency(account.currentCycleDebit) }}</span>
            </div>
          </div>
        </div>

        <!-- Customer Information -->
        <div v-if="customer" class="section">
          <h2>Customer Information</h2>
          <div class="details-grid">
            <div class="detail-item">
              <label>Customer ID:</label>
              <span>{{ customer.customerId }}</span>
            </div>
            <div class="detail-item">
              <label>Name:</label>
              <span>{{ formatCustomerName(customer) }}</span>
            </div>
            <div class="detail-item">
              <label>Address:</label>
              <span class="address">
                {{ formatAddress(customer) }}
              </span>
            </div>
            <div class="detail-item">
              <label>Phone 1:</label>
              <span>{{ customer.phoneNumber1 || 'N/A' }}</span>
            </div>
            <div class="detail-item">
              <label>Phone 2:</label>
              <span>{{ customer.phoneNumber2 || 'N/A' }}</span>
            </div>
            <div class="detail-item">
              <label>Date of Birth:</label>
              <span>{{ customer.dateOfBirth || 'N/A' }}</span>
            </div>
            <div class="detail-item">
              <label>FICO Score:</label>
              <span>{{ customer.ficoCreditScore || 'N/A' }}</span>
            </div>
          </div>
        </div>

        <!-- Cards Information -->
        <div v-if="cards && cards.length > 0" class="section">
          <h2>Associated Cards</h2>
          <div class="cards-table">
            <table>
              <thead>
                <tr>
                  <th>Card Number</th>
                  <th>Status</th>
                  <th>Expiration Date</th>
                  <th>Issue Date</th>
                </tr>
              </thead>
              <tbody>
                <tr v-for="card in cards" :key="card.cardNumber">
                  <td>{{ maskCardNumber(card.cardNumber) }}</td>
                  <td>{{ card.cardStatus }}</td>
                  <td>{{ card.expirationDate || 'N/A' }}</td>
                  <td>{{ card.issueDate || 'N/A' }}</td>
                </tr>
              </tbody>
            </table>
          </div>
        </div>

        <!-- Action Buttons -->
        <div class="action-buttons">
          <button class="btn btn-primary" @click="navigateToUpdate">
            Update Account
          </button>
          <button class="btn btn-secondary" @click="returnToMenu">
            Return to Menu
          </button>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import { ref, computed } from 'vue'
import { useRouter } from 'vue-router'
import { useAccountStore } from '../stores/account'
import accountService from '../services/accountService'
import FormInput from '../components/FormInput.vue'
import LoadingSpinner from '../components/LoadingSpinner.vue'
import ErrorMessage from '../components/ErrorMessage.vue'

export default {
  name: 'AccountView',
  components: {
    FormInput,
    LoadingSpinner,
    ErrorMessage
  },
  setup() {
    const router = useRouter()
    const accountStore = useAccountStore()
    
    const accountId = ref('')
    const loading = ref(false)
    const errorMessage = ref('')
    const validationError = ref('')
    
    const account = computed(() => accountStore.currentAccount)
    const customer = computed(() => accountStore.currentCustomer)
    const cards = computed(() => accountStore.currentCards)

    /**
     * Validate account ID format
     * Requirements: 3.1, 3.2, 3.3, 3.4
     */
    const validateAccountId = (id) => {
      if (!id || id.trim() === '') {
        return 'Account number not provided'
      }
      
      if (!/^\d{11}$/.test(id)) {
        return 'Account number must be a non zero 11 digit number'
      }
      
      if (id === '00000000000') {
        return 'Account number must be a non zero 11 digit number'
      }
      
      return null
    }

    /**
     * Search for account by ID
     * Requirements: 3.5, 3.6, 3.7, 3.8, 3.9
     */
    const searchAccount = async () => {
      clearError()
      
      // Validate account ID
      const error = validateAccountId(accountId.value)
      if (error) {
        validationError.value = error
        return
      }
      
      loading.value = true
      accountStore.setLoading(true)
      
      try {
        // Fetch account data
        const accountData = await accountService.getAccount(accountId.value)
        accountStore.setAccount(accountData)
        
        // Fetch customer data
        try {
          const customerData = await accountService.getCustomer(accountId.value)
          accountStore.setCustomer(customerData)
        } catch (error) {
          // Handle customer not found error
          if (error.message.includes('customer')) {
            errorMessage.value = 'Did not find associated customer in master file'
          } else {
            throw error
          }
        }
        
        // Fetch cards data
        try {
          const cardsData = await accountService.getCards(accountId.value)
          accountStore.setCards(cardsData)
        } catch (error) {
          // Cards are optional, just log the error
          console.warn('Could not fetch cards:', error)
          accountStore.setCards([])
        }
        
      } catch (error) {
        // Handle account not found error
        if (error.status === 404 || error.message.includes('not found')) {
          errorMessage.value = 'Did not find this account in account master file'
        } else {
          errorMessage.value = error.message || 'An error occurred while searching for the account'
        }
        accountStore.clearAccount()
      } finally {
        loading.value = false
        accountStore.setLoading(false)
      }
    }

    /**
     * Navigate to account update page
     * Requirements: 3.11
     */
    const navigateToUpdate = () => {
      router.push({
        name: 'AccountUpdate',
        params: { accountId: accountId.value }
      })
    }

    /**
     * Return to main menu
     * Requirements: 3.11
     */
    const returnToMenu = () => {
      accountStore.clearAccount()
      router.push({ name: 'MainMenu' })
    }

    /**
     * Clear error messages
     */
    const clearError = () => {
      errorMessage.value = ''
      validationError.value = ''
    }

    /**
     * Format currency values
     */
    const formatCurrency = (value) => {
      if (value === null || value === undefined) return '$0.00'
      const num = typeof value === 'string' ? parseFloat(value) : value
      return new Intl.NumberFormat('en-US', {
        style: 'currency',
        currency: 'USD'
      }).format(num)
    }

    /**
     * Format customer name
     */
    const formatCustomerName = (customer) => {
      const parts = [
        customer.firstName,
        customer.middleName,
        customer.lastName
      ].filter(part => part && part.trim())
      return parts.join(' ')
    }

    /**
     * Format customer address
     */
    const formatAddress = (customer) => {
      const parts = [
        customer.addressLine1,
        customer.addressLine2,
        customer.addressLine3,
        [customer.stateCode, customer.zipCode].filter(Boolean).join(' ')
      ].filter(part => part && part.trim())
      return parts.join(', ')
    }

    /**
     * Mask card number (show only last 4 digits)
     */
    const maskCardNumber = (cardNumber) => {
      if (!cardNumber) return 'N/A'
      const str = String(cardNumber)
      if (str.length < 4) return str
      return '**** **** **** ' + str.slice(-4)
    }

    return {
      accountId,
      loading,
      errorMessage,
      validationError,
      account,
      customer,
      cards,
      searchAccount,
      navigateToUpdate,
      returnToMenu,
      clearError,
      formatCurrency,
      formatCustomerName,
      formatAddress,
      maskCardNumber
    }
  }
}
</script>

<style scoped>
.account-view {
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
  font-size: 1.5rem;
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
  display: flex;
  align-items: center;
  justify-content: center;
  min-width: 120px;
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

.btn:disabled {
  opacity: 0.6;
  cursor: not-allowed;
}

.account-details {
  background: white;
  padding: 2rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.section {
  margin-bottom: 2rem;
}

.section:last-of-type {
  margin-bottom: 0;
}

.section h2 {
  margin-top: 0;
  margin-bottom: 1.5rem;
  color: #333;
  font-size: 1.5rem;
  border-bottom: 2px solid #007bff;
  padding-bottom: 0.5rem;
}

.details-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 1rem;
}

.detail-item {
  display: flex;
  flex-direction: column;
  gap: 0.25rem;
}

.detail-item label {
  font-weight: 600;
  color: #666;
  font-size: 0.875rem;
}

.detail-item span {
  color: #333;
  font-size: 1rem;
}

.currency {
  font-weight: 600;
  color: #007bff;
}

.status-active {
  color: #28a745;
  font-weight: 600;
}

.status-inactive {
  color: #dc3545;
  font-weight: 600;
}

.address {
  line-height: 1.5;
}

.cards-table {
  overflow-x: auto;
}

table {
  width: 100%;
  border-collapse: collapse;
}

thead {
  background-color: #f8f9fa;
}

th {
  padding: 0.75rem;
  text-align: left;
  font-weight: 600;
  color: #333;
  border-bottom: 2px solid #dee2e6;
}

td {
  padding: 0.75rem;
  border-bottom: 1px solid #dee2e6;
  color: #666;
}

tbody tr:hover {
  background-color: #f8f9fa;
}

.action-buttons {
  display: flex;
  gap: 1rem;
  margin-top: 2rem;
  padding-top: 2rem;
  border-top: 1px solid #dee2e6;
}

@media (max-width: 768px) {
  .container {
    padding: 1rem;
  }
  
  .details-grid {
    grid-template-columns: 1fr;
  }
  
  .button-group,
  .action-buttons {
    flex-direction: column;
  }
  
  .btn {
    width: 100%;
  }
}
</style>
