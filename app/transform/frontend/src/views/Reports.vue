<template>
  <div class="reports">
    <div class="container">
      <ErrorMessage v-if="errorMessage" :message="errorMessage" @close="errorMessage = ''" />

      <!-- Report Type Selection -->
      <div class="report-selection">
        <h3>Select Report Type</h3>
        <div class="report-types">
          <button 
            class="report-type-btn"
            :class="{ active: selectedReportType === 'accounts' }"
            @click="selectReportType('accounts')"
          >
            <span class="icon">📊</span>
            <span class="label">Account Report</span>
          </button>
          <button 
            class="report-type-btn"
            :class="{ active: selectedReportType === 'transactions' }"
            @click="selectReportType('transactions')"
          >
            <span class="icon">💳</span>
            <span class="label">Transaction Report</span>
          </button>
          <button 
            class="report-type-btn"
            :class="{ active: selectedReportType === 'cards' }"
            @click="selectReportType('cards')"
          >
            <span class="icon">🎴</span>
            <span class="label">Card Report</span>
          </button>
        </div>
      </div>

      <!-- Report Filters -->
      <div v-if="selectedReportType" class="report-filters">
        <h3>Report Parameters</h3>
        
        <!-- Account Report Filters -->
        <div v-if="selectedReportType === 'accounts'" class="filter-group">
          <div class="form-group">
            <label for="accountStatus">Account Status:</label>
            <select id="accountStatus" v-model="filters.accounts.status" class="form-control">
              <option value="">All</option>
              <option value="Y">Active</option>
              <option value="N">Inactive</option>
            </select>
          </div>
          <div class="form-group">
            <label for="accountStartDate">Open Date From:</label>
            <input 
              id="accountStartDate" 
              v-model="filters.accounts.startDate" 
              type="date" 
              class="form-control"
            />
          </div>
          <div class="form-group">
            <label for="accountEndDate">Open Date To:</label>
            <input 
              id="accountEndDate" 
              v-model="filters.accounts.endDate" 
              type="date" 
              class="form-control"
            />
          </div>
        </div>

        <!-- Transaction Report Filters -->
        <div v-if="selectedReportType === 'transactions'" class="filter-group">
          <div class="form-group">
            <label for="txnStartDate">Transaction Date From:</label>
            <input 
              id="txnStartDate" 
              v-model="filters.transactions.startDate" 
              type="date" 
              class="form-control"
            />
          </div>
          <div class="form-group">
            <label for="txnEndDate">Transaction Date To:</label>
            <input 
              id="txnEndDate" 
              v-model="filters.transactions.endDate" 
              type="date" 
              class="form-control"
            />
          </div>
          <div class="form-group">
            <label for="txnType">Transaction Type:</label>
            <input 
              id="txnType" 
              v-model="filters.transactions.type" 
              type="text" 
              class="form-control"
              placeholder="e.g., PURCHASE, PAYMENT"
            />
          </div>
        </div>

        <!-- Card Report Filters -->
        <div v-if="selectedReportType === 'cards'" class="filter-group">
          <div class="form-group">
            <label for="cardStatus">Card Status:</label>
            <select id="cardStatus" v-model="filters.cards.status" class="form-control">
              <option value="">All</option>
              <option value="ACTIVE">Active</option>
              <option value="INACTIVE">Inactive</option>
              <option value="EXPIRED">Expired</option>
            </select>
          </div>
          <div class="form-group">
            <label for="expiringBefore">Expiring Before:</label>
            <input 
              id="expiringBefore" 
              v-model="filters.cards.expiringBefore" 
              type="date" 
              class="form-control"
            />
          </div>
        </div>

        <div class="button-group">
          <button 
            class="btn btn-primary" 
            @click="generateReport"
            :disabled="loading"
          >
            <LoadingSpinner v-if="loading" size="small" />
            <span v-else>Generate Report</span>
          </button>
          <button 
            class="btn btn-secondary" 
            @click="clearFilters"
            :disabled="loading"
          >
            Clear Filters
          </button>
        </div>
      </div>

      <!-- Report Display -->
      <div v-if="reportData" class="report-display">
        <!-- Account Report -->
        <div v-if="selectedReportType === 'accounts'" class="report-content">
          <h3>Account Report</h3>
          <div class="report-summary">
            <div class="summary-card">
              <div class="summary-label">Total Accounts</div>
              <div class="summary-value">{{ reportData.totalAccounts }}</div>
            </div>
            <div class="summary-card">
              <div class="summary-label">Active Accounts</div>
              <div class="summary-value">{{ reportData.activeAccounts }}</div>
            </div>
            <div class="summary-card">
              <div class="summary-label">Inactive Accounts</div>
              <div class="summary-value">{{ reportData.inactiveAccounts }}</div>
            </div>
            <div class="summary-card">
              <div class="summary-label">Total Balance</div>
              <div class="summary-value">${{ formatAmount(reportData.totalBalance) }}</div>
            </div>
            <div class="summary-card">
              <div class="summary-label">Total Credit Limit</div>
              <div class="summary-value">${{ formatAmount(reportData.totalCreditLimit) }}</div>
            </div>
            <div class="summary-card">
              <div class="summary-label">Average Balance</div>
              <div class="summary-value">${{ formatAmount(reportData.averageBalance) }}</div>
            </div>
          </div>

          <div v-if="reportData.accounts && reportData.accounts.length > 0" class="report-table">
            <h4>Account Details</h4>
            <table>
              <thead>
                <tr>
                  <th>Account ID</th>
                  <th>Status</th>
                  <th>Balance</th>
                  <th>Credit Limit</th>
                  <th>Open Date</th>
                  <th>Expiration Date</th>
                </tr>
              </thead>
              <tbody>
                <tr v-for="account in reportData.accounts" :key="account.accountId">
                  <td>{{ account.accountId }}</td>
                  <td>{{ account.activeStatus === 'Y' ? 'Active' : 'Inactive' }}</td>
                  <td>${{ formatAmount(account.currentBalance) }}</td>
                  <td>${{ formatAmount(account.creditLimit) }}</td>
                  <td>{{ formatDate(account.openDate) }}</td>
                  <td>{{ formatDate(account.expirationDate) }}</td>
                </tr>
              </tbody>
            </table>
          </div>
        </div>

        <!-- Transaction Report -->
        <div v-if="selectedReportType === 'transactions'" class="report-content">
          <h3>Transaction Report</h3>
          <div class="report-summary">
            <div class="summary-card">
              <div class="summary-label">Total Transactions</div>
              <div class="summary-value">{{ reportData.totalTransactions }}</div>
            </div>
            <div class="summary-card">
              <div class="summary-label">Total Amount</div>
              <div class="summary-value">${{ formatAmount(reportData.totalAmount) }}</div>
            </div>
            <div class="summary-card">
              <div class="summary-label">Average Amount</div>
              <div class="summary-value">${{ formatAmount(reportData.averageAmount) }}</div>
            </div>
            <div class="summary-card">
              <div class="summary-label">Max Amount</div>
              <div class="summary-value">${{ formatAmount(reportData.maxAmount) }}</div>
            </div>
            <div class="summary-card">
              <div class="summary-label">Min Amount</div>
              <div class="summary-value">${{ formatAmount(reportData.minAmount) }}</div>
            </div>
          </div>

          <div v-if="reportData.transactions && reportData.transactions.length > 0" class="report-table">
            <h4>Transaction Details</h4>
            <table>
              <thead>
                <tr>
                  <th>Transaction ID</th>
                  <th>Account ID</th>
                  <th>Card Number</th>
                  <th>Type</th>
                  <th>Amount</th>
                  <th>Date</th>
                  <th>Description</th>
                </tr>
              </thead>
              <tbody>
                <tr v-for="txn in reportData.transactions" :key="txn.transactionId">
                  <td>{{ txn.transactionId }}</td>
                  <td>{{ txn.accountId }}</td>
                  <td>{{ maskCardNumber(txn.cardNumber) }}</td>
                  <td>{{ txn.transactionType }}</td>
                  <td>${{ formatAmount(txn.amount) }}</td>
                  <td>{{ formatDate(txn.transactionDate) }}</td>
                  <td>{{ txn.description }}</td>
                </tr>
              </tbody>
            </table>
          </div>
        </div>

        <!-- Card Report -->
        <div v-if="selectedReportType === 'cards'" class="report-content">
          <h3>Card Report</h3>
          <div class="report-summary">
            <div class="summary-card">
              <div class="summary-label">Total Cards</div>
              <div class="summary-value">{{ reportData.totalCards }}</div>
            </div>
            <div class="summary-card">
              <div class="summary-label">Active Cards</div>
              <div class="summary-value">{{ reportData.activeCards }}</div>
            </div>
            <div class="summary-card">
              <div class="summary-label">Inactive Cards</div>
              <div class="summary-value">{{ reportData.inactiveCards }}</div>
            </div>
            <div class="summary-card">
              <div class="summary-label">Expiring Cards</div>
              <div class="summary-value">{{ reportData.expiringCards }}</div>
            </div>
          </div>

          <div v-if="reportData.cards && reportData.cards.length > 0" class="report-table">
            <h4>Card Details</h4>
            <table>
              <thead>
                <tr>
                  <th>Card Number</th>
                  <th>Account ID</th>
                  <th>Customer ID</th>
                  <th>Status</th>
                  <th>Issue Date</th>
                  <th>Expiration Date</th>
                </tr>
              </thead>
              <tbody>
                <tr v-for="card in reportData.cards" :key="card.cardNumber">
                  <td>{{ maskCardNumber(card.cardNumber) }}</td>
                  <td>{{ card.accountId }}</td>
                  <td>{{ card.customerId }}</td>
                  <td>{{ card.cardStatus }}</td>
                  <td>{{ formatDate(card.issueDate) }}</td>
                  <td>{{ formatDate(card.expirationDate) }}</td>
                </tr>
              </tbody>
            </table>
          </div>
        </div>

        <div class="button-group">
          <button class="btn btn-secondary" @click="$router.push('/menu')">
            Return to Menu
          </button>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import ErrorMessage from '../components/ErrorMessage.vue'
import LoadingSpinner from '../components/LoadingSpinner.vue'
import reportService from '../services/reportService'

export default {
  name: 'Reports',
  components: {
    ErrorMessage,
    LoadingSpinner
  },
  data() {
    return {
      selectedReportType: null,
      filters: {
        accounts: {
          status: '',
          startDate: '',
          endDate: ''
        },
        transactions: {
          startDate: '',
          endDate: '',
          type: ''
        },
        cards: {
          status: '',
          expiringBefore: ''
        }
      },
      reportData: null,
      errorMessage: '',
      loading: false
    }
  },
  methods: {
    selectReportType(type) {
      this.selectedReportType = type
      this.reportData = null
      this.errorMessage = ''
    },

    async generateReport() {
      this.loading = true
      this.errorMessage = ''
      this.reportData = null

      try {
        if (this.selectedReportType === 'accounts') {
          this.reportData = await reportService.generateAccountReport(this.filters.accounts)
        } else if (this.selectedReportType === 'transactions') {
          this.reportData = await reportService.generateTransactionReport(this.filters.transactions)
        } else if (this.selectedReportType === 'cards') {
          this.reportData = await reportService.generateCardReport(this.filters.cards)
        }
      } catch (error) {
        console.error('Error generating report:', error)
        this.errorMessage = error.response?.data?.message || 'Failed to generate report'
      } finally {
        this.loading = false
      }
    },

    clearFilters() {
      if (this.selectedReportType === 'accounts') {
        this.filters.accounts = { status: '', startDate: '', endDate: '' }
      } else if (this.selectedReportType === 'transactions') {
        this.filters.transactions = { startDate: '', endDate: '', type: '' }
      } else if (this.selectedReportType === 'cards') {
        this.filters.cards = { status: '', expiringBefore: '' }
      }
      this.reportData = null
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
        month: 'short', 
        day: 'numeric' 
      })
    },

    maskCardNumber(cardNumber) {
      if (!cardNumber) return 'N/A'
      const cardStr = cardNumber.toString()
      if (cardStr.length < 4) return cardStr
      return '**** **** **** ' + cardStr.slice(-4)
    }
  }
}
</script>

<style scoped>
.reports {
  min-height: 100vh;
  background-color: #f5f5f5;
}

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 2rem;
  background: white;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.report-selection {
  margin-bottom: 2rem;
}

h3 {
  margin-top: 0;
  margin-bottom: 1rem;
  color: #333;
  font-size: 1.25rem;
}

.report-types {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
}

.report-type-btn {
  display: flex;
  flex-direction: column;
  align-items: center;
  padding: 1.5rem;
  border: 2px solid #dee2e6;
  border-radius: 8px;
  background-color: white;
  cursor: pointer;
  transition: all 0.2s;
}

.report-type-btn:hover {
  border-color: #007bff;
  background-color: #f8f9fa;
}

.report-type-btn.active {
  border-color: #007bff;
  background-color: #e7f3ff;
}

.report-type-btn .icon {
  font-size: 2rem;
  margin-bottom: 0.5rem;
}

.report-type-btn .label {
  font-size: 1rem;
  font-weight: 600;
  color: #333;
}

.report-filters {
  margin-bottom: 2rem;
  padding: 1.5rem;
  border: 1px solid #dee2e6;
  border-radius: 4px;
  background-color: #f8f9fa;
}

.filter-group {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 1rem;
  margin-bottom: 1rem;
}

.form-group {
  margin-bottom: 0;
}

label {
  display: block;
  margin-bottom: 0.5rem;
  font-weight: 600;
  color: #495057;
  font-size: 0.875rem;
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

.report-display {
  margin-top: 2rem;
}

.report-content h3 {
  margin-bottom: 1.5rem;
  color: #007bff;
}

.report-summary {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 1rem;
  margin-bottom: 2rem;
}

.summary-card {
  padding: 1.5rem;
  border: 1px solid #dee2e6;
  border-radius: 8px;
  background-color: #f8f9fa;
  text-align: center;
}

.summary-label {
  font-size: 0.875rem;
  color: #6c757d;
  margin-bottom: 0.5rem;
}

.summary-value {
  font-size: 1.5rem;
  font-weight: 600;
  color: #007bff;
}

.report-table {
  margin-top: 2rem;
}

.report-table h4 {
  margin-bottom: 1rem;
  color: #333;
}

table {
  width: 100%;
  border-collapse: collapse;
  background-color: white;
}

thead {
  background-color: #007bff;
  color: white;
}

th {
  padding: 0.75rem;
  text-align: left;
  font-weight: 600;
}

td {
  padding: 0.75rem;
  border-bottom: 1px solid #dee2e6;
}

tbody tr:hover {
  background-color: #f8f9fa;
}

tbody tr:last-child td {
  border-bottom: none;
}
</style>
