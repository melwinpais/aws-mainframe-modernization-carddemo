import api from './api'
import { useAuthStore } from '@/stores/auth'

/**
 * Authentication Service
 * Handles user authentication, session management, and token storage
 * Requirements: 1.1, 1.2, 13.1, 13.2, 13.3, 13.4
 */
const authService = {
  /**
   * Authenticate user with credentials
   * POST /api/auth/login
   * Requirements: 1.1, 1.2
   * 
   * @param {string} userId - User ID (8 characters, will be converted to uppercase)
   * @param {string} password - Password (8 characters)
   * @returns {Promise<Object>} Login response with token and user info
   * @throws {Error} Authentication error with message
   */
  async login(userId, password) {
    try {
      const response = await api.post('/auth/login', {
        userId: userId.trim().toUpperCase(), // Normalize user ID to uppercase
        password: password.trim()
      })
      
      const authData = response.data
      
      // Store authentication data in store and localStorage
      const authStore = useAuthStore()
      authStore.setAuth({
        token: authData.token,
        userId: authData.userId,
        userType: authData.userType,
        firstName: authData.firstName,
        lastName: authData.lastName
      })
      
      return authData
    } catch (error) {
      // Extract error message from response
      const errorMessage = error.response?.data?.message || 
                          error.response?.data?.error ||
                          'Authentication failed. Please try again.'
      throw new Error(errorMessage)
    }
  },

  /**
   * Logout current user
   * POST /api/auth/logout
   * Requirements: 13.1, 13.2
   * 
   * @returns {Promise<void>}
   */
  async logout() {
    try {
      // Call logout endpoint to invalidate token on server
      await api.post('/auth/logout')
    } catch (error) {
      // Log error but don't throw - we still want to clear local state
      console.error('Logout error:', error)
    } finally {
      // Always clear local authentication state
      const authStore = useAuthStore()
      authStore.clearAuth()
    }
  },

  /**
   * Validate current session
   * GET /api/auth/validate
   * Requirements: 13.3, 13.4
   * 
   * @returns {Promise<Object>} Session validation response
   * @throws {Error} If session is invalid or expired
   */
  async validateSession() {
    try {
      const response = await api.get('/auth/validate')
      return response.data
    } catch (error) {
      // Session is invalid - clear local state
      const authStore = useAuthStore()
      authStore.clearAuth()
      
      const errorMessage = error.response?.data?.message || 
                          'Session expired. Please login again.'
      throw new Error(errorMessage)
    }
  },

  /**
   * Get current authentication token from localStorage
   * Requirements: 13.2
   * 
   * @returns {string|null} JWT token or null if not authenticated
   */
  getToken() {
    return localStorage.getItem('token')
  },

  /**
   * Set authentication token in localStorage
   * Requirements: 13.2
   * 
   * @param {string} token - JWT token
   */
  setToken(token) {
    localStorage.setItem('token', token)
  },

  /**
   * Clear authentication token from localStorage
   * Requirements: 13.2
   */
  clearToken() {
    localStorage.removeItem('token')
  },

  /**
   * Get current user information from localStorage
   * Requirements: 13.2
   * 
   * @returns {Object|null} User info object or null if not authenticated
   */
  getUserInfo() {
    const userId = localStorage.getItem('userId')
    const userType = localStorage.getItem('userType')
    const firstName = localStorage.getItem('firstName')
    const lastName = localStorage.getItem('lastName')
    
    if (!userId) {
      return null
    }
    
    return {
      userId,
      userType,
      firstName,
      lastName
    }
  },

  /**
   * Check if user is currently authenticated
   * 
   * @returns {boolean} True if user has valid token
   */
  isAuthenticated() {
    return !!this.getToken()
  },

  /**
   * Check if current user is an admin
   * 
   * @returns {boolean} True if user type is 'A'
   */
  isAdmin() {
    const userInfo = this.getUserInfo()
    return userInfo?.userType === 'A'
  }
}

export default authService
