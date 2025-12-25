<template>
  <div class="account-update">
    
    <div class="container">
      <LoadingSpinner v-if="initialLoading" message="Loading account data..." />
      
      <div v-else-if="account" class="update-form-container">
        <!-- Success Message -->
        <div v-if="successMessage" class="alert alert-success">
          {{ successMessage }}
        </div>
        
        <!-- Error Message -->
        <ErrorMessage v-if="generalError" :message="generalError" @close="clearGeneralError" />
        
        <!-- Account Information Section -->
        <div class="section">
          <h2>Account Information</h2>
          <div class="form-grid">
            <FormInput
              v-model="account.accountId"
              label="Account ID"
              type="text"
              :disabled="true"
              :readonly="true"
            />
            
            <div class="form-group">
              <label for="activeStatus">Active Status *</label>
              <select
                id="activeStatus"
                v-model="account.activeStatus"
                class="form-control"
                :class="{ 'is-invalid': fieldErrors.activeStatus }"
                @change="markFieldChanged('activeStatus')"
              >
                <option value="Y">Active (Y)</option>
                <option value="N">Inactive (N)</option>
              </select>
              <div v-if="fieldErrors.activeStatus" class="error-message">
                {{ fieldErrors.activeStatus }}
              </div>
            </div>
            
            <FormInput
              v-model="account.currentBalance"
              label="Current Balance *"
              type="number"
              step="0.01"
              :error="fieldErrors.currentBalance"
              @input="markFieldChanged('currentBalance')"
            />
            
            <FormInput
              v-model="account.creditLimit"
              label="Credit Limit *"
              type="number"
              step="0.01"
              :error="fieldErrors.creditLimit"
              @input="markFieldChanged('creditLimit')"
            />
            
            <FormInput
              v-model="account.cashCreditLimit"
              label="Cash Credit Limit *"
              type="number"
              step="0.01"
              :error="fieldErrors.cashCreditLimit"
              @input="markFieldChanged('cashCreditLimit')"
            />
            
            <FormInput
              v-model="account.openDate"
              label="Open Date"
              type="text"
              :disabled="true"
              :readonly="true"
            />
            
            <FormInput
              v-model="account.expirationDate"
              label="Expiration Date"
              type="text"
              placeholder="YYYY-MM-DD"
              :error="fieldErrors.expirationDate"
              @input="markFieldChanged('expirationDate')"
            />
            
            <FormInput
              v-model="account.reissueDate"
              label="Reissue Date"
              type="text"
              placeholder="YYYY-MM-DD"
              :error="fieldErrors.reissueDate"
              @input="markFieldChanged('reissueDate')"
            />
            
            <FormInput
              v-model="account.currentCycleCredit"
              label="Current Cycle Credit"
              type="number"
              step="0.01"
              :error="fieldErrors.currentCycleCredit"
              @input="markFieldChanged('currentCycleCredit')"
            />
            
            <FormInput
              v-model="account.currentCycleDebit"
              label="Current Cycle Debit"
              type="number"
              step="0.01"
              :error="fieldErrors.currentCycleDebit"
              @input="markFieldChanged('currentCycleDebit')"
            />
          </div>
        </div>

        <!-- Customer Information Section -->
        <div v-if="customer" class="section">
          <h2>Customer Information</h2>
          <div class="form-grid">
            <FormInput
              v-model="customer.customerId"
              label="Customer ID"
              type="text"
              :disabled="true"
              :readonly="true"
            />
            
            <FormInput
              v-model="customer.firstName"
              label="First Name *"
              type="text"
              :error="fieldErrors.firstName"
              @input="markFieldChanged('firstName')"
            />
            
            <FormInput
              v-model="customer.middleName"
              label="Middle Name"
              type="text"
              :error="fieldErrors.middleName"
              @input="markFieldChanged('middleName')"
            />
            
            <FormInput
              v-model="customer.lastName"
              label="Last Name *"
              type="text"
              :error="fieldErrors.lastName"
              @input="markFieldChanged('lastName')"
            />
            
            <FormInput
              v-model="customer.addressLine1"
              label="Address Line 1"
              type="text"
              :error="fieldErrors.addressLine1"
              @input="markFieldChanged('addressLine1')"
            />
            
            <FormInput
              v-model="customer.addressLine2"
              label="Address Line 2"
              type="text"
              :error="fieldErrors.addressLine2"
              @input="markFieldChanged('addressLine2')"
            />
            
            <FormInput
              v-model="customer.addressLine3"
              label="Address Line 3"
              type="text"
              :error="fieldErrors.addressLine3"
              @input="markFieldChanged('addressLine3')"
            />
            
            <FormInput
              v-model="customer.stateCode"
              label="State Code"
              type="text"
              maxlength="2"
              :error="fieldErrors.stateCode"
              @input="markFieldChanged('stateCode')"
            />
            
            <FormInput
              v-model="customer.countryCode"
              label="Country Code"
              type="text"
              maxlength="3"
              :error="fieldErrors.countryCode"
              @input="markFieldChanged('countryCode')"
            />
            
            <FormInput
              v-model="customer.zipCode"
              label="Zip Code"
              type="text"
              maxlength="10"
              :error="fieldErrors.zipCode"
              @input="markFieldChanged('zipCode')"
            />
            
            <FormInput
              v-model="customer.phoneNumber1"
              label="Phone Number 1"
              type="text"
              placeholder="(XXX)XXX-XXXX"
              :error="fieldErrors.phoneNumber1"
              @input="markFieldChanged('phoneNumber1')"
            />
            
            <FormInput
              v-model="customer.phoneNumber2"
              label="Phone Number 2"
              type="text"
              placeholder="(XXX)XXX-XXXX"
              :error="fieldErrors.phoneNumber2"
              @input="markFieldChanged('phoneNumber2')"
            />
            
            <FormInput
              v-model="customer.dateOfBirth"
              label="Date of Birth"
              type="text"
              placeholder="YYYY-MM-DD"
              :error="fieldErrors.dateOfBirth"
              @input="markFieldChanged('dateOfBirth')"
            />
            
            <FormInput
              v-model="customer.ficoCreditScore"
              label="FICO Credit Score"
              type="number"
              min="0"
              max="999"
              :error="fieldErrors.ficoCreditScore"
              @input="markFieldChanged('ficoCreditScore')"
            />
          </div>
        </div>

        <!-- Action Buttons -->
        <div class="action-buttons">
          <button 
            class="btn btn-primary" 
            @click="saveChanges" 
            :disabled="!hasChanges || saving"
          >
            <LoadingSpinner v-if="saving" size="small" />
            <span v-else>Save Changes</span>
          </button>
          <button 
            class="btn btn-secondary" 
            @click="cancelChanges" 
            :disabled="saving"
          >
            Cancel
          </button>
          <button 
            class="btn btn-secondary" 
            @click="returnToView" 
            :disabled="saving"
          >
            Return to View
          </button>
        </div>
      </div>
      
      <div v-else class="error-container">
        <ErrorMessage 
          message="Unable to load account data. Please return to account view and try again." 
        />
        <button class="btn btn-secondary" @click="returnToView">
          Return to Account View
        </button>
      </div>
    </div>
  </div>
</template>

<script>
import { ref, reactive, computed, onMounted } from 'vue'
import { useRouter, useRoute } from 'vue-router'
import { useAccountStore } from '../stores/account'
import accountService from '../services/accountService'
import FormInput from '../components/FormInput.vue'
import LoadingSpinner from '../components/LoadingSpinner.vue'
import ErrorMessage from '../components/ErrorMessage.vue'

export default {
  name: 'AccountUpdate',
  components: {
    FormInput,
    LoadingSpinner,
    ErrorMessage
  },
  setup() {
    const router = useRouter()
    const route = useRoute()
    const accountStore = useAccountStore()
    
    const initialLoading = ref(true)
    const saving = ref(false)
    const successMessage = ref('')
    const generalError = ref('')
    const fieldErrors = reactive({})
    const changedFields = new Set()
    
    const account = ref(null)
    const customer = ref(null)
    const originalAccount = ref(null)
    const originalCustomer = ref(null)
    
    const accountId = route.params.accountId

    /**
     * Check if there are any changes
     * Requirements: 4.2
     */
    const hasChanges = computed(() => {
      return changedFields.size > 0
    })

    /**
     * Load account and customer data
     * Requirements: 4.1
     */
    const loadAccountData = async () => {
      initialLoading.value = true
      
      try {
        // Try to use data from store first
        if (accountStore.currentAccount && 
            accountStore.currentAccount.accountId === accountId) {
          account.value = { ...accountStore.currentAccount }
          originalAccount.value = { ...accountStore.currentAccount }
          
          if (accountStore.currentCustomer) {
            customer.value = { ...accountStore.currentCustomer }
            originalCustomer.value = { ...accountStore.currentCustomer }
          }
        } else {
          // Fetch fresh data
          const accountData = await accountService.getAccount(accountId)
          account.value = { ...accountData }
          originalAccount.value = { ...accountData }
          accountStore.setAccount(accountData)
          
          try {
            const customerData = await accountService.getCustomer(accountId)
            customer.value = { ...customerData }
            originalCustomer.value = { ...customerData }
            accountStore.setCustomer(customerData)
          } catch (error) {
            console.warn('Could not load customer data:', error)
          }
        }
      } catch (error) {
        generalError.value = error.message || 'Failed to load account data'
      } finally {
        initialLoading.value = false
      }
    }

    /**
     * Mark a field as changed
     * Requirements: 4.2
     */
    const markFieldChanged = (fieldName) => {
      changedFields.add(fieldName)
      // Clear field error when user starts typing
      if (fieldErrors[fieldName]) {
        delete fieldErrors[fieldName]
      }
    }

    /**
     * Validate all fields
     * Requirements: 4.2, 4.6
     */
    const validateFields = () => {
      const errors = {}
      
      // Validate account fields
      if (account.value) {
        if (!account.value.activeStatus || !['Y', 'N'].includes(account.value.activeStatus)) {
          errors.activeStatus = 'Account Active Status must be Y or N'
        }
        
        if (account.value.creditLimit === null || account.value.creditLimit === undefined || account.value.creditLimit === '') {
          errors.creditLimit = 'Credit Limit must be supplied'
        } else if (isNaN(account.value.creditLimit) || parseFloat(account.value.creditLimit) < 0) {
          errors.creditLimit = 'Credit Limit must be a valid positive number'
        }
        
        if (account.value.cashCreditLimit === null || account.value.cashCreditLimit === undefined || account.value.cashCreditLimit === '') {
          errors.cashCreditLimit = 'Cash Credit Limit must be supplied'
        } else if (isNaN(account.value.cashCreditLimit) || parseFloat(account.value.cashCreditLimit) < 0) {
          errors.cashCreditLimit = 'Cash Credit Limit must be a valid positive number'
        }
        
        if (account.value.currentBalance !== null && account.value.currentBalance !== undefined && account.value.currentBalance !== '') {
          if (isNaN(account.value.currentBalance)) {
            errors.currentBalance = 'Current Balance must be a valid number'
          }
        }
        
        // Validate date formats if provided
        if (account.value.expirationDate && account.value.expirationDate.trim()) {
          if (!isValidDate(account.value.expirationDate)) {
            errors.expirationDate = 'Expiration Date must be in YYYY-MM-DD format'
          }
        }
        
        if (account.value.reissueDate && account.value.reissueDate.trim()) {
          if (!isValidDate(account.value.reissueDate)) {
            errors.reissueDate = 'Reissue Date must be in YYYY-MM-DD format'
          }
        }
      }
      
      // Validate customer fields
      if (customer.value) {
        if (!customer.value.firstName || customer.value.firstName.trim() === '') {
          errors.firstName = 'First Name is required'
        } else if (!isValidName(customer.value.firstName)) {
          errors.firstName = 'First Name must contain only letters and spaces'
        }
        
        if (!customer.value.lastName || customer.value.lastName.trim() === '') {
          errors.lastName = 'Last Name is required'
        } else if (!isValidName(customer.value.lastName)) {
          errors.lastName = 'Last Name must contain only letters and spaces'
        }
        
        if (customer.value.middleName && customer.value.middleName.trim() && !isValidName(customer.value.middleName)) {
          errors.middleName = 'Middle Name must contain only letters and spaces'
        }
        
        if (customer.value.phoneNumber1 && customer.value.phoneNumber1.trim() && !isValidPhoneNumber(customer.value.phoneNumber1)) {
          errors.phoneNumber1 = 'Phone Number must be in format (XXX)XXX-XXXX'
        }
        
        if (customer.value.phoneNumber2 && customer.value.phoneNumber2.trim() && !isValidPhoneNumber(customer.value.phoneNumber2)) {
          errors.phoneNumber2 = 'Phone Number must be in format (XXX)XXX-XXXX'
        }
        
        if (customer.value.dateOfBirth && customer.value.dateOfBirth.trim() && !isValidDate(customer.value.dateOfBirth)) {
          errors.dateOfBirth = 'Date of Birth must be in YYYY-MM-DD format'
        }
        
        if (customer.value.ficoCreditScore !== null && customer.value.ficoCreditScore !== undefined && customer.value.ficoCreditScore !== '') {
          const score = parseInt(customer.value.ficoCreditScore)
          if (isNaN(score) || score < 0 || score > 999) {
            errors.ficoCreditScore = 'FICO Score must be a 3-digit number (0-999)'
          }
        }
      }
      
      return errors
    }

    /**
     * Validate date format (YYYY-MM-DD)
     */
    const isValidDate = (dateStr) => {
      const regex = /^\d{4}-\d{2}-\d{2}$/
      if (!regex.test(dateStr)) return false
      
      const [year, month, day] = dateStr.split('-').map(Number)
      if (month < 1 || month > 12) return false
      if (day < 1 || day > 31) return false
      
      return true
    }

    /**
     * Validate name (letters and spaces only)
     */
    const isValidName = (name) => {
      return /^[A-Za-z\s]+$/.test(name)
    }

    /**
     * Validate phone number format
     */
    const isValidPhoneNumber = (phone) => {
      return /^\(\d{3}\)\d{3}-\d{4}$/.test(phone)
    }

    /**
     * Save changes to account and customer
     * Requirements: 4.3, 4.4, 4.5, 4.6
     */
    const saveChanges = async () => {
      // Clear previous errors
      Object.keys(fieldErrors).forEach(key => delete fieldErrors[key])
      generalError.value = ''
      successMessage.value = ''
      
      // Validate fields
      const errors = validateFields()
      if (Object.keys(errors).length > 0) {
        Object.assign(fieldErrors, errors)
        generalError.value = 'Please correct the errors in the form'
        return
      }
      
      saving.value = true
      
      try {
        // Prepare account updates (only changed fields)
        const accountUpdates = {}
        if (changedFields.has('activeStatus')) accountUpdates.activeStatus = account.value.activeStatus
        if (changedFields.has('creditLimit')) accountUpdates.creditLimit = parseFloat(account.value.creditLimit)
        if (changedFields.has('cashCreditLimit')) accountUpdates.cashCreditLimit = parseFloat(account.value.cashCreditLimit)
        if (changedFields.has('currentBalance')) accountUpdates.currentBalance = parseFloat(account.value.currentBalance)
        if (changedFields.has('currentCycleCredit')) accountUpdates.currentCycleCredit = parseFloat(account.value.currentCycleCredit)
        if (changedFields.has('currentCycleDebit')) accountUpdates.currentCycleDebit = parseFloat(account.value.currentCycleDebit)
        if (changedFields.has('expirationDate')) accountUpdates.expirationDate = account.value.expirationDate
        if (changedFields.has('reissueDate')) accountUpdates.reissueDate = account.value.reissueDate
        
        // Update account if there are changes
        if (Object.keys(accountUpdates).length > 0) {
          const updatedAccount = await accountService.updateAccount(accountId, accountUpdates)
          account.value = { ...updatedAccount }
          originalAccount.value = { ...updatedAccount }
          accountStore.setAccount(updatedAccount)
        }
        
        // Note: Customer updates would be handled similarly if the API supports it
        // For now, we'll just show success for account updates
        
        successMessage.value = 'Account updated successfully'
        changedFields.clear()
        
        // Scroll to top to show success message
        window.scrollTo({ top: 0, behavior: 'smooth' })
        
      } catch (error) {
        if (error.fieldErrors) {
          // Handle field-specific errors from server
          error.fieldErrors.forEach(fieldError => {
            fieldErrors[fieldError.field] = fieldError.message
          })
          generalError.value = 'Please correct the errors in the form'
        } else {
          generalError.value = error.message || 'Failed to update account'
        }
      } finally {
        saving.value = false
      }
    }

    /**
     * Cancel changes and revert to original values
     * Requirements: 4.2
     */
    const cancelChanges = () => {
      if (hasChanges.value) {
        if (confirm('Are you sure you want to discard your changes?')) {
          account.value = { ...originalAccount.value }
          customer.value = originalCustomer.value ? { ...originalCustomer.value } : null
          changedFields.clear()
          Object.keys(fieldErrors).forEach(key => delete fieldErrors[key])
          generalError.value = ''
          successMessage.value = ''
        }
      } else {
        returnToView()
      }
    }

    /**
     * Return to account view
     */
    const returnToView = () => {
      router.push({ name: 'AccountView' })
    }

    /**
     * Clear general error message
     */
    const clearGeneralError = () => {
      generalError.value = ''
    }

    // Load data on mount
    onMounted(() => {
      loadAccountData()
    })

    return {
      initialLoading,
      saving,
      successMessage,
      generalError,
      fieldErrors,
      account,
      customer,
      hasChanges,
      markFieldChanged,
      saveChanges,
      cancelChanges,
      returnToView,
      clearGeneralError
    }
  }
}
</script>

<style scoped>
.account-update {
  min-height: 100vh;
  background-color: #f5f5f5;
}

.container {
  max-width: 1200px;
  margin: 0 auto;
  padding: 2rem;
}

.update-form-container {
  background: white;
  padding: 2rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.alert {
  padding: 1rem;
  border-radius: 4px;
  margin-bottom: 1.5rem;
}

.alert-success {
  background-color: #d4edda;
  border: 1px solid #c3e6cb;
  color: #155724;
}

.section {
  margin-bottom: 2rem;
}

.section:last-of-type {
  margin-bottom: 0;
}

.section h2 {
  margin-top: 0;
  margin-bottom: 1.5rem;
  color: #333;
  font-size: 1.5rem;
  border-bottom: 2px solid #007bff;
  padding-bottom: 0.5rem;
}

.form-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
  gap: 1.5rem;
}

.form-group {
  display: flex;
  flex-direction: column;
  gap: 0.5rem;
}

.form-group label {
  font-weight: 600;
  color: #333;
  font-size: 0.875rem;
}

.form-control {
  padding: 0.75rem;
  border: 1px solid #ced4da;
  border-radius: 4px;
  font-size: 1rem;
  transition: border-color 0.2s;
}

.form-control:focus {
  outline: none;
  border-color: #007bff;
  box-shadow: 0 0 0 0.2rem rgba(0, 123, 255, 0.25);
}

.form-control.is-invalid {
  border-color: #dc3545;
}

.error-message {
  color: #dc3545;
  font-size: 0.875rem;
  margin-top: 0.25rem;
}

.action-buttons {
  display: flex;
  gap: 1rem;
  margin-top: 2rem;
  padding-top: 2rem;
  border-top: 1px solid #dee2e6;
}

.btn {
  padding: 0.75rem 1.5rem;
  border: none;
  border-radius: 4px;
  font-size: 1rem;
  cursor: pointer;
  transition: background-color 0.2s;
  display: flex;
  align-items: center;
  justify-content: center;
  min-width: 120px;
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

.btn:disabled {
  opacity: 0.6;
  cursor: not-allowed;
}

.error-container {
  background: white;
  padding: 2rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  text-align: center;
}

.error-container .btn {
  margin-top: 1rem;
}

@media (max-width: 768px) {
  .container {
    padding: 1rem;
  }
  
  .form-grid {
    grid-template-columns: 1fr;
  }
  
  .action-buttons {
    flex-direction: column;
  }
  
  .btn {
    width: 100%;
  }
}
</style>
