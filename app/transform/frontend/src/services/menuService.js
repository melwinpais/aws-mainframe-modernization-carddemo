import api from './api'

/**
 * Menu Service
 * Handles menu navigation operations including retrieving menu options
 * and validating menu selections.
 */

/**
 * Get menu options for the authenticated user.
 * Returns different menu options based on user type (Admin vs Regular User).
 * 
 * @returns {Promise<Array>} Array of menu options
 * @throws {Error} If the request fails
 */
export async function getOptions() {
  try {
    const response = await api.get('/menu/options')
    return response.data
  } catch (error) {
    console.error('Error fetching menu options:', error)
    throw error
  }
}

/**
 * Validate and process a menu selection.
 * Validates that the option is valid and the user has permission to access it.
 * 
 * @param {number} optionNumber - The selected menu option number
 * @returns {Promise<Object>} Menu selection response with programName and route
 * @throws {Error} If the request fails or validation fails
 */
export async function selectOption(optionNumber) {
  try {
    const response = await api.post('/menu/select', {
      optionNumber
    })
    return response.data
  } catch (error) {
    console.error('Error selecting menu option:', error)
    throw error
  }
}

export default {
  getOptions,
  selectOption
}
