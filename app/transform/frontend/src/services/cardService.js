import api from './api'

/**
 * Card Service
 * Handles all card-related API calls
 */

/**
 * Get card details by card number
 * @param {string} cardNumber - 16-digit card number
 * @returns {Promise<Object>} Card details
 */
export async function getCard(cardNumber) {
  try {
    const response = await api.get(`/cards/${cardNumber}`)
    return response.data
  } catch (error) {
    throw error
  }
}

/**
 * Get all cards for an account
 * @param {string} accountId - 11-digit account ID
 * @returns {Promise<Array>} List of cards
 */
export async function getCardsByAccount(accountId) {
  try {
    const response = await api.get(`/cards/account/${accountId}`)
    return response.data
  } catch (error) {
    throw error
  }
}

/**
 * Create a new card
 * @param {Object} cardData - Card creation data
 * @returns {Promise<Object>} Created card details
 */
export async function createCard(cardData) {
  try {
    const response = await api.post('/cards', cardData)
    return response.data
  } catch (error) {
    throw error
  }
}

/**
 * Update card information
 * @param {string} cardNumber - 16-digit card number
 * @param {Object} updates - Card update data
 * @returns {Promise<Object>} Updated card details
 */
export async function updateCard(cardNumber, updates) {
  try {
    const response = await api.put(`/cards/${cardNumber}`, updates)
    return response.data
  } catch (error) {
    throw error
  }
}

/**
 * Deactivate a card
 * @param {string} cardNumber - 16-digit card number
 * @returns {Promise<Object>} Deactivation confirmation
 */
export async function deactivateCard(cardNumber) {
  try {
    const response = await api.delete(`/cards/${cardNumber}`)
    return response.data
  } catch (error) {
    throw error
  }
}

export default {
  getCard,
  getCardsByAccount,
  createCard,
  updateCard,
  deactivateCard
}
