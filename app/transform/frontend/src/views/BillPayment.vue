<template>
  <div class="bill-payment">
    <div class="container">
      <ErrorMessage v-if="errorMessage" :message="errorMessage" @close="errorMessage = ''" />
      
      <div v-if="successMessage" class="success-message">
        <p>{{ successMessage }}</p>
        <button class="btn btn-primary" @click="resetForm">Make Another Payment</button>
        <button class="btn btn-secondary" @click="$router.push('/menu')">Return to Menu</button>
      </div>

      <div v-else>
        <!-- Account ID Input -->
        <div class="form-section">
          <h3>Account Information</h3>
          <div class="form-group">
            <label for="accountId">Account ID:</label>
            <input
              id="accountId"
              v-model="accountId"
              type="text"
              class="form-control"
              placeholder="Enter 11-digit account ID"
              maxlength="11"
              @blur="loadBillInfo"
              :disabled="loading"
            />
          </div>
          <button 
            class="btn btn-primary" 
            @click="loadBillInfo"
            :disabled="loading || !accountId"
          >
            Load Bill Information
          </button>
        </div>

        <!-- Bill Information Display -->
        <div v-if="billInfo" class="bill-info-section">
          <h3>Bill Information</h3>
          <div class="info-grid">
            <div class="info-item">
              <label>Account ID:</label>
              <span>{{ billInfo.accountId }}</span>
            </div>
            <div class="info-item">
              <label>Current Balance:</label>
              <span class="amount">${{ formatAmount(billInfo.currentBalance) }}</span>
            </div>
            <div class="info-item">
              <label>Minimum Payment:</label>
              <span class="amount">${{ formatAmount(billInfo.minimumPayment) }}</span>
            </div>
            <div class="info-item">
              <label>Due Date:</label>
              <span>{{ formatDate(billInfo.dueDate) }}</span>
            </div>
          </div>
        </div>

        <!-- Payment Form -->
        <div v-if="billInfo" class="payment-form-section">
          <h3>Payment Details</h3>
          <div class="form-group">
            <label for="paymentAmount">Payment Amount:</label>
            <div class="input-with-prefix">
              <span class="prefix">$</span>
              <input
                id="paymentAmount"
                v-model="paymentAmount"
                type="number"
                step="0.01"
                min="0.01"
                class="form-control"
                placeholder="0.00"
                :disabled="loading"
                @input="validatePaymentAmount"
              />
            </div>
            <div v-if="paymentAmountError" class="field-error">{{ paymentAmountError }}</div>
            <div class="quick-amounts">
              <button 
                class="btn btn-sm btn-outline" 
                @click="setPaymentAmount(billInfo.minimumPayment)"
                :disabled="loading"
              >
                Minimum Payment
              </button>
              <button 
                class="btn btn-sm btn-outline" 
                @click="setPaymentAmount(billInfo.currentBalance)"
                :disabled="loading"
              >
                Full Balance
              </button>
            </div>
          </div>

          <div class="form-group">
            <label for="paymentDate">Payment Date:</label>
            <input
              id="paymentDate"
              v-model="paymentDate"
              type="date"
              class="form-control"
              :disabled="loading"
              :max="maxDate"
            />
          </div>

          <div class="button-group">
            <button 
              class="btn btn-primary" 
              @click="processPayment"
              :disabled="loading || !isPaymentValid"
            >
              <LoadingSpinner v-if="loading" size="small" />
              <span v-else>Process Payment</span>
            </button>
            <button 
              class="btn btn-secondary" 
              @click="$router.push('/menu')"
              :disabled="loading"
            >
              Cancel
            </button>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import ErrorMessage from '../components/ErrorMessage.vue'
import LoadingSpinner from '../components/LoadingSpinner.vue'
import billPaymentService from '../services/billPaymentService'

export default {
  name: 'BillPayment',
  components: {
    ErrorMessage,
    LoadingSpinner
  },
  data() {
    return {
      accountId: '',
      billInfo: null,
      paymentAmount: '',
      paymentDate: this.getTodayDate(),
      paymentAmountError: '',
      errorMessage: '',
      successMessage: '',
      loading: false
    }
  },
  computed: {
    isPaymentValid() {
      return this.paymentAmount && 
             parseFloat(this.paymentAmount) > 0 && 
             !this.paymentAmountError &&
             this.paymentDate
    },
    maxDate() {
      // Allow payments up to 30 days in the future
      const date = new Date()
      date.setDate(date.getDate() + 30)
      return date.toISOString().split('T')[0]
    }
  },
  methods: {
    getTodayDate() {
      return new Date().toISOString().split('T')[0]
    },
    
    async loadBillInfo() {
      if (!this.accountId) {
        this.errorMessage = 'Please enter an account ID'
        return
      }

      // Validate account ID format (11 digits)
      if (!/^\d{11}$/.test(this.accountId)) {
        this.errorMessage = 'Account ID must be 11 digits'
        return
      }

      this.loading = true
      this.errorMessage = ''
      this.billInfo = null

      try {
        this.billInfo = await billPaymentService.getBillInfo(this.accountId)
      } catch (error) {
        console.error('Error loading bill info:', error)
        this.errorMessage = error.response?.data?.message || 'Failed to load bill information'
      } finally {
        this.loading = false
      }
    },

    validatePaymentAmount() {
      this.paymentAmountError = ''
      
      if (!this.paymentAmount) {
        return
      }

      const amount = parseFloat(this.paymentAmount)
      
      if (isNaN(amount) || amount <= 0) {
        this.paymentAmountError = 'Payment amount must be greater than 0'
        return
      }

      if (amount > parseFloat(this.billInfo.currentBalance)) {
        this.paymentAmountError = 'Payment amount cannot exceed current balance'
        return
      }

      // Check for valid decimal places (max 2)
      const decimalPart = this.paymentAmount.toString().split('.')[1]
      if (decimalPart && decimalPart.length > 2) {
        this.paymentAmountError = 'Payment amount can have at most 2 decimal places'
        return
      }
    },

    setPaymentAmount(amount) {
      this.paymentAmount = parseFloat(amount).toFixed(2)
      this.validatePaymentAmount()
    },

    async processPayment() {
      if (!this.isPaymentValid) {
        return
      }

      this.loading = true
      this.errorMessage = ''

      try {
        const paymentData = {
          accountId: parseInt(this.accountId),
          amount: parseFloat(this.paymentAmount),
          paymentDate: this.paymentDate
        }

        const transaction = await billPaymentService.processBillPayment(paymentData)
        
        this.successMessage = `Payment processed successfully! Transaction ID: ${transaction.transactionId}. Amount: $${this.formatAmount(transaction.amount)}`
      } catch (error) {
        console.error('Error processing payment:', error)
        this.errorMessage = error.response?.data?.message || 'Failed to process payment'
      } finally {
        this.loading = false
      }
    },

    resetForm() {
      this.accountId = ''
      this.billInfo = null
      this.paymentAmount = ''
      this.paymentDate = this.getTodayDate()
      this.paymentAmountError = ''
      this.errorMessage = ''
      this.successMessage = ''
    },

    formatAmount(amount) {
      if (amount === null || amount === undefined) return '0.00'
      return parseFloat(amount).toFixed(2)
    },

    formatDate(dateString) {
      if (!dateString) return 'N/A'
      const date = new Date(dateString)
      return date.toLocaleDateString('en-US', { 
        year: 'numeric', 
        month: 'long', 
        day: 'numeric' 
      })
    }
  }
}
</script>

<style scoped>
.bill-payment {
  min-height: 100vh;
  background-color: #f5f5f5;
}

.container {
  max-width: 800px;
  margin: 0 auto;
  padding: 2rem;
  background: white;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.form-section,
.bill-info-section,
.payment-form-section {
  margin-bottom: 2rem;
  padding: 1.5rem;
  border: 1px solid #dee2e6;
  border-radius: 4px;
  background-color: #f8f9fa;
}

h3 {
  margin-top: 0;
  margin-bottom: 1rem;
  color: #333;
  font-size: 1.25rem;
}

.form-group {
  margin-bottom: 1.5rem;
}

label {
  display: block;
  margin-bottom: 0.5rem;
  font-weight: 600;
  color: #495057;
}

.form-control {
  width: 100%;
  padding: 0.75rem;
  border: 1px solid #ced4da;
  border-radius: 4px;
  font-size: 1rem;
}

.form-control:focus {
  outline: none;
  border-color: #007bff;
  box-shadow: 0 0 0 0.2rem rgba(0, 123, 255, 0.25);
}

.form-control:disabled {
  background-color: #e9ecef;
  cursor: not-allowed;
}

.input-with-prefix {
  display: flex;
  align-items: center;
}

.prefix {
  padding: 0.75rem;
  background-color: #e9ecef;
  border: 1px solid #ced4da;
  border-right: none;
  border-radius: 4px 0 0 4px;
  font-weight: 600;
}

.input-with-prefix .form-control {
  border-radius: 0 4px 4px 0;
}

.field-error {
  color: #dc3545;
  font-size: 0.875rem;
  margin-top: 0.25rem;
}

.info-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
}

.info-item {
  display: flex;
  flex-direction: column;
}

.info-item label {
  font-size: 0.875rem;
  color: #6c757d;
  margin-bottom: 0.25rem;
}

.info-item span {
  font-size: 1rem;
  color: #212529;
}

.info-item .amount {
  font-weight: 600;
  font-size: 1.125rem;
  color: #007bff;
}

.quick-amounts {
  display: flex;
  gap: 0.5rem;
  margin-top: 0.5rem;
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

.btn-outline {
  background-color: white;
  color: #007bff;
  border: 1px solid #007bff;
}

.btn-outline:hover:not(:disabled) {
  background-color: #007bff;
  color: white;
}

.button-group {
  display: flex;
  gap: 1rem;
  margin-top: 1.5rem;
}

.success-message {
  padding: 1.5rem;
  background-color: #d4edda;
  border: 1px solid #c3e6cb;
  border-radius: 4px;
  color: #155724;
}

.success-message p {
  margin-bottom: 1rem;
  font-size: 1.125rem;
}

.success-message .button-group {
  margin-top: 1rem;
}
</style>
