import { describe, it, expect, vi, beforeEach } from 'vitest'
import { mount } from '@vue/test-utils'
import { createPinia, setActivePinia } from 'pinia'
import Login from './Login.vue'
import authService from '@/services/authService'

// Mock the router
const mockRouter = {
  push: vi.fn()
}

vi.mock('vue-router', () => ({
  useRouter: () => mockRouter
}))

// Mock the authService
vi.mock('@/services/authService', () => ({
  default: {
    login: vi.fn()
  }
}))

describe('Login Component', () => {
  let wrapper

  beforeEach(() => {
    // Create a fresh pinia instance for each test
    setActivePinia(createPinia())
    
    // Clear all mocks before each test
    vi.clearAllMocks()
    
    // Mount the component
    wrapper = mount(Login, {
      global: {
        stubs: {
          // Stub any child components if needed
        }
      }
    })
  })

  /**
   * Test successful login flow
   * Requirements: 1.1, 1.2, 1.7, 1.8
   */
  describe('Successful Login', () => {
    it('should login successfully with valid credentials and navigate to main menu for regular user', async () => {
      // Arrange
      const mockResponse = {
        token: 'test-token-123',
        userId: 'TESTUSER',
        userType: 'U',
        firstName: 'Test',
        lastName: 'User'
      }
      authService.login.mockResolvedValue(mockResponse)

      // Act
      await wrapper.find('#userId').setValue('testuser')
      await wrapper.find('#password').setValue('password')
      await wrapper.find('form').trigger('submit.prevent')
      
      // Wait for async operations
      await wrapper.vm.$nextTick()
      await new Promise(resolve => setTimeout(resolve, 0))

      // Assert
      expect(authService.login).toHaveBeenCalledWith('testuser', 'password')
      expect(mockRouter.push).toHaveBeenCalledWith('/menu')
      expect(wrapper.vm.errorMessage).toBe('')
    })

    it('should login successfully and navigate to admin menu for admin user', async () => {
      // Arrange
      const mockResponse = {
        token: 'admin-token-123',
        userId: 'ADMIN001',
        userType: 'A',
        firstName: 'Admin',
        lastName: 'User'
      }
      authService.login.mockResolvedValue(mockResponse)

      // Act
      await wrapper.find('#userId').setValue('admin001')
      await wrapper.find('#password').setValue('adminpass')
      await wrapper.find('form').trigger('submit.prevent')
      
      // Wait for async operations
      await wrapper.vm.$nextTick()
      await new Promise(resolve => setTimeout(resolve, 0))

      // Assert
      expect(authService.login).toHaveBeenCalledWith('admin001', 'adminpass')
      expect(mockRouter.push).toHaveBeenCalledWith('/admin-menu')
      expect(wrapper.vm.errorMessage).toBe('')
    })
  })

  /**
   * Test login with invalid credentials
   * Requirements: 1.3, 1.4
   */
  describe('Invalid Credentials', () => {
    it('should display error message when user is not found', async () => {
      // Arrange
      const errorMessage = 'User not found. Try again ...'
      authService.login.mockRejectedValue(new Error(errorMessage))

      // Act
      await wrapper.find('#userId').setValue('invalid')
      await wrapper.find('#password').setValue('password')
      await wrapper.find('form').trigger('submit.prevent')
      
      // Wait for async operations
      await wrapper.vm.$nextTick()
      await new Promise(resolve => setTimeout(resolve, 0))

      // Assert
      expect(authService.login).toHaveBeenCalledWith('invalid', 'password')
      expect(mockRouter.push).not.toHaveBeenCalled()
      expect(wrapper.vm.errorMessage).toBe(errorMessage)
      expect(wrapper.find('.error-message').text()).toBe(errorMessage)
    })

    it('should display error message when password is wrong', async () => {
      // Arrange
      const errorMessage = 'Wrong Password. Try again ...'
      authService.login.mockRejectedValue(new Error(errorMessage))

      // Act
      await wrapper.find('#userId').setValue('testuser')
      await wrapper.find('#password').setValue('wrongpass')
      await wrapper.find('form').trigger('submit.prevent')
      
      // Wait for async operations
      await wrapper.vm.$nextTick()
      await new Promise(resolve => setTimeout(resolve, 0))

      // Assert
      expect(authService.login).toHaveBeenCalledWith('testuser', 'wrongpass')
      expect(mockRouter.push).not.toHaveBeenCalled()
      expect(wrapper.vm.errorMessage).toBe(errorMessage)
      expect(wrapper.find('.error-message').text()).toBe(errorMessage)
    })
  })

  /**
   * Test empty field validation
   * Requirements: 1.5, 1.6
   */
  describe('Empty Field Validation', () => {
    it('should display error when user ID is empty', async () => {
      // Act
      await wrapper.find('#userId').setValue('')
      await wrapper.find('#password').setValue('password')
      await wrapper.find('form').trigger('submit.prevent')
      
      // Wait for validation
      await wrapper.vm.$nextTick()

      // Assert
      expect(authService.login).not.toHaveBeenCalled()
      expect(wrapper.vm.errorMessage).toBe('Please enter User ID ...')
      expect(wrapper.find('.error-message').text()).toBe('Please enter User ID ...')
    })

    it('should display error when user ID contains only spaces', async () => {
      // Act
      await wrapper.find('#userId').setValue('   ')
      await wrapper.find('#password').setValue('password')
      await wrapper.find('form').trigger('submit.prevent')
      
      // Wait for validation
      await wrapper.vm.$nextTick()

      // Assert
      expect(authService.login).not.toHaveBeenCalled()
      expect(wrapper.vm.errorMessage).toBe('Please enter User ID ...')
    })

    it('should display error when password is empty', async () => {
      // Act
      await wrapper.find('#userId').setValue('testuser')
      await wrapper.find('#password').setValue('')
      await wrapper.find('form').trigger('submit.prevent')
      
      // Wait for validation
      await wrapper.vm.$nextTick()

      // Assert
      expect(authService.login).not.toHaveBeenCalled()
      expect(wrapper.vm.errorMessage).toBe('Please enter Password ...')
      expect(wrapper.find('.error-message').text()).toBe('Please enter Password ...')
    })

    it('should display error when password contains only spaces', async () => {
      // Act
      await wrapper.find('#userId').setValue('testuser')
      await wrapper.find('#password').setValue('   ')
      await wrapper.find('form').trigger('submit.prevent')
      
      // Wait for validation
      await wrapper.vm.$nextTick()

      // Assert
      expect(authService.login).not.toHaveBeenCalled()
      expect(wrapper.vm.errorMessage).toBe('Please enter Password ...')
    })

    it('should display error when both fields are empty', async () => {
      // Act
      await wrapper.find('#userId').setValue('')
      await wrapper.find('#password').setValue('')
      await wrapper.find('form').trigger('submit.prevent')
      
      // Wait for validation
      await wrapper.vm.$nextTick()

      // Assert
      expect(authService.login).not.toHaveBeenCalled()
      expect(wrapper.vm.errorMessage).toBe('Please enter User ID ...')
    })
  })

  /**
   * Test loading state
   */
  describe('Loading State', () => {
    it('should show loading state during login', async () => {
      // Arrange
      let resolveLogin
      const loginPromise = new Promise(resolve => {
        resolveLogin = resolve
      })
      authService.login.mockReturnValue(loginPromise)

      // Act
      await wrapper.find('#userId').setValue('testuser')
      await wrapper.find('#password').setValue('password')
      await wrapper.find('form').trigger('submit.prevent')
      await wrapper.vm.$nextTick()

      // Assert - button should be disabled and show loading text
      expect(wrapper.find('button').attributes('disabled')).toBeDefined()
      expect(wrapper.find('button').text()).toBe('Signing in...')

      // Resolve the login
      resolveLogin({
        token: 'test-token',
        userId: 'TESTUSER',
        userType: 'U',
        firstName: 'Test',
        lastName: 'User'
      })
      await wrapper.vm.$nextTick()
      await new Promise(resolve => setTimeout(resolve, 0))

      // Assert - button should be enabled again
      expect(wrapper.find('button').attributes('disabled')).toBeUndefined()
      expect(wrapper.find('button').text()).toBe('Sign In')
    })
  })
})
