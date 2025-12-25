import api from './api'

/**
 * Transaction Service
 * Handles all transaction-related API calls
 */
const transactionService = {
  /**
   * Get a single transaction by ID
   * @param {number} transactionId - The transaction ID
   * @returns {Promise} Transaction data
   */
  async getTransaction(transactionId) {
    try {
      const response = await api.get(`/transactions/${transactionId}`)
      return response.data
    } catch (error) {
      console.error('Error fetching transaction:', error)
      throw error
    }
  },

  /**
   * Get transactions by account ID with optional filters
   * @param {number} accountId - The account ID
   * @param {Object} params - Query parameters
   * @param {string} params.startDate - Start date filter (YYYY-MM-DD)
   * @param {string} params.endDate - End date filter (YYYY-MM-DD)
   * @param {number} params.page - Page number (0-indexed)
   * @param {number} params.size - Page size
   * @returns {Promise} Paginated transaction data
   */
  async getTransactionsByAccount(accountId, params = {}) {
    try {
      const queryParams = {
        page: params.page || 0,
        size: params.size || 20
      }
      
      if (params.startDate) {
        queryParams.startDate = params.startDate
      }
      
      if (params.endDate) {
        queryParams.endDate = params.endDate
      }
      
      const response = await api.get(`/transactions/account/${accountId}`, {
        params: queryParams
      })
      return response.data
    } catch (error) {
      console.error('Error fetching transactions by account:', error)
      throw error
    }
  },

  /**
   * Get transactions by card number with optional filters
   * @param {string} cardNumber - The card number
   * @param {Object} params - Query parameters
   * @param {string} params.startDate - Start date filter (YYYY-MM-DD)
   * @param {string} params.endDate - End date filter (YYYY-MM-DD)
   * @param {number} params.page - Page number (0-indexed)
   * @param {number} params.size - Page size
   * @returns {Promise} Paginated transaction data
   */
  async getTransactionsByCard(cardNumber, params = {}) {
    try {
      const queryParams = {
        page: params.page || 0,
        size: params.size || 20
      }
      
      if (params.startDate) {
        queryParams.startDate = params.startDate
      }
      
      if (params.endDate) {
        queryParams.endDate = params.endDate
      }
      
      const response = await api.get(`/transactions/card/${cardNumber}`, {
        params: queryParams
      })
      return response.data
    } catch (error) {
      console.error('Error fetching transactions by card:', error)
      throw error
    }
  },

  /**
   * Create a new transaction
   * @param {Object} transactionData - Transaction data
   * @param {number} transactionData.accountId - Account ID
   * @param {string} transactionData.cardNumber - Card number (optional)
   * @param {string} transactionData.transactionType - Transaction type
   * @param {string} transactionData.transactionCategory - Transaction category (optional)
   * @param {number} transactionData.amount - Transaction amount
   * @param {string} transactionData.transactionDate - Transaction date (YYYY-MM-DD)
   * @param {string} transactionData.transactionTime - Transaction time (HH:MM:SS) (optional)
   * @param {string} transactionData.description - Description (optional)
   * @param {string} transactionData.merchantName - Merchant name (optional)
   * @param {string} transactionData.merchantCity - Merchant city (optional)
   * @param {string} transactionData.merchantZip - Merchant zip (optional)
   * @returns {Promise} Created transaction data
   */
  async createTransaction(transactionData) {
    try {
      const response = await api.post('/transactions', transactionData)
      return response.data
    } catch (error) {
      console.error('Error creating transaction:', error)
      throw error
    }
  }
}

export default transactionService
