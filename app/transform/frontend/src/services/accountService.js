import api from './api'

/**
 * Account Service
 * Handles all account-related API calls
 */
const accountService = {
  /**
   * Get account details by account ID
   * @param {string|number} accountId - The account ID
   * @returns {Promise<Object>} Account data
   */
  async getAccount(accountId) {
    try {
      const response = await api.get(`/accounts/${accountId}`)
      return response.data
    } catch (error) {
      throw this.handleError(error)
    }
  },

  /**
   * Update account information
   * @param {string|number} accountId - The account ID
   * @param {Object} updates - Account update data
   * @returns {Promise<Object>} Updated account data
   */
  async updateAccount(accountId, updates) {
    try {
      const response = await api.put(`/accounts/${accountId}`, updates)
      return response.data
    } catch (error) {
      throw this.handleError(error)
    }
  },

  /**
   * Get customer information for an account
   * @param {string|number} accountId - The account ID
   * @returns {Promise<Object>} Customer data
   */
  async getCustomer(accountId) {
    try {
      const response = await api.get(`/accounts/${accountId}/customer`)
      return response.data
    } catch (error) {
      throw this.handleError(error)
    }
  },

  /**
   * Get all cards associated with an account
   * @param {string|number} accountId - The account ID
   * @returns {Promise<Array>} Array of card data
   */
  async getCards(accountId) {
    try {
      const response = await api.get(`/accounts/${accountId}/cards`)
      return response.data
    } catch (error) {
      throw this.handleError(error)
    }
  },

  /**
   * Search accounts by criteria
   * @param {Object} params - Search parameters (accountId, customerId)
   * @returns {Promise<Array>} Array of account data
   */
  async searchAccounts(params) {
    try {
      const response = await api.get('/accounts/search', { params })
      return response.data
    } catch (error) {
      throw this.handleError(error)
    }
  },

  /**
   * Handle API errors and extract meaningful error messages
   * @param {Error} error - The error object
   * @returns {Error} Formatted error
   */
  handleError(error) {
    if (error.response) {
      // Server responded with error status
      const { data } = error.response
      
      // Check for field-specific errors
      if (data.fieldErrors && data.fieldErrors.length > 0) {
        const fieldError = data.fieldErrors[0]
        const err = new Error(fieldError.message || data.message || 'An error occurred')
        err.fieldErrors = data.fieldErrors
        err.status = error.response.status
        return err
      }
      
      // General error message
      const err = new Error(data.message || 'An error occurred')
      err.status = error.response.status
      return err
    } else if (error.request) {
      // Request made but no response received
      return new Error('No response from server. Please check your connection.')
    } else {
      // Something else happened
      return new Error(error.message || 'An unexpected error occurred')
    }
  }
}

export default accountService
