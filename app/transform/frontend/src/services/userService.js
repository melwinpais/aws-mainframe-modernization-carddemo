import api from './api'

/**
 * User Service
 * Handles all user management API calls (Admin Only)
 */
const userService = {
  /**
   * List all users with pagination
   * @param {string} userType - Optional filter by user type ('A' or 'U')
   * @param {number} page - Page number (0-indexed)
   * @param {number} size - Page size
   * @returns {Promise<Object>} Paginated user data
   */
  async listUsers(userType = null, page = 0, size = 20) {
    try {
      const params = { page, size }
      if (userType) {
        params.userType = userType
      }
      const response = await api.get('/users', { params })
      return response.data
    } catch (error) {
      throw this.handleError(error)
    }
  },

  /**
   * Get user details by user ID
   * @param {string} userId - The user ID
   * @returns {Promise<Object>} User data
   */
  async getUser(userId) {
    try {
      const response = await api.get(`/users/${userId}`)
      return response.data
    } catch (error) {
      throw this.handleError(error)
    }
  },

  /**
   * Create a new user
   * @param {Object} userData - User creation data
   * @param {string} userData.userId - User ID (8 characters)
   * @param {string} userData.firstName - First name
   * @param {string} userData.lastName - Last name
   * @param {string} userData.password - Password (8 characters)
   * @param {string} userData.userType - User type ('A' or 'U')
   * @returns {Promise<Object>} Created user data
   */
  async createUser(userData) {
    try {
      const response = await api.post('/users', userData)
      return response.data
    } catch (error) {
      throw this.handleError(error)
    }
  },

  /**
   * Update user information
   * @param {string} userId - The user ID
   * @param {Object} updates - User update data
   * @returns {Promise<Object>} Updated user data
   */
  async updateUser(userId, updates) {
    try {
      const response = await api.put(`/users/${userId}`, updates)
      return response.data
    } catch (error) {
      throw this.handleError(error)
    }
  },

  /**
   * Deactivate a user
   * @param {string} userId - The user ID
   * @returns {Promise<Object>} Deactivation confirmation message
   */
  async deactivateUser(userId) {
    try {
      const response = await api.delete(`/users/${userId}`)
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

export default userService
