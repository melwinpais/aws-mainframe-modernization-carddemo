import api from './api'

/**
 * Report Service
 * Handles report generation operations
 */
export default {
  /**
   * Generate account report
   * @param {object} filters - Report filters
   * @param {string} filters.status - Account status filter ('Y' or 'N')
   * @param {string} filters.startDate - Start date filter (YYYY-MM-DD)
   * @param {string} filters.endDate - End date filter (YYYY-MM-DD)
   * @returns {Promise} Account report data
   */
  async generateAccountReport(filters = {}) {
    const params = new URLSearchParams()
    if (filters.status) params.append('status', filters.status)
    if (filters.startDate) params.append('startDate', filters.startDate)
    if (filters.endDate) params.append('endDate', filters.endDate)
    
    const response = await api.get(`/reports/accounts?${params.toString()}`)
    return response.data
  },

  /**
   * Generate transaction report
   * @param {object} filters - Report filters
   * @param {string} filters.startDate - Start date filter (YYYY-MM-DD)
   * @param {string} filters.endDate - End date filter (YYYY-MM-DD)
   * @param {string} filters.type - Transaction type filter
   * @returns {Promise} Transaction report data
   */
  async generateTransactionReport(filters = {}) {
    const params = new URLSearchParams()
    if (filters.startDate) params.append('startDate', filters.startDate)
    if (filters.endDate) params.append('endDate', filters.endDate)
    if (filters.type) params.append('type', filters.type)
    
    const response = await api.get(`/reports/transactions?${params.toString()}`)
    return response.data
  },

  /**
   * Generate card report
   * @param {object} filters - Report filters
   * @param {string} filters.status - Card status filter
   * @param {string} filters.expiringBefore - Expiring before date (YYYY-MM-DD)
   * @returns {Promise} Card report data
   */
  async generateCardReport(filters = {}) {
    const params = new URLSearchParams()
    if (filters.status) params.append('status', filters.status)
    if (filters.expiringBefore) params.append('expiringBefore', filters.expiringBefore)
    
    const response = await api.get(`/reports/cards?${params.toString()}`)
    return response.data
  }
}
