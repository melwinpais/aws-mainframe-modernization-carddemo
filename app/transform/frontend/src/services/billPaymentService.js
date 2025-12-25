import api from './api'

/**
 * Bill Payment Service
 * Handles bill payment operations
 */
export default {
  /**
   * Get bill information for an account
   * @param {number} accountId - The account ID
   * @returns {Promise} Bill information
   */
  async getBillInfo(accountId) {
    const response = await api.get(`/bills/account/${accountId}`)
    return response.data
  },

  /**
   * Process a bill payment
   * @param {object} paymentData - Payment details
   * @param {number} paymentData.accountId - Account ID
   * @param {number} paymentData.amount - Payment amount
   * @param {string} paymentData.paymentDate - Payment date (YYYY-MM-DD)
   * @returns {Promise} Transaction details
   */
  async processBillPayment(paymentData) {
    const response = await api.post('/bills/payment', paymentData)
    return response.data
  }
}
