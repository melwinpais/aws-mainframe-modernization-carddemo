<template>
  <div class="user-management">
    <div class="container">
      <LoadingSpinner v-if="initialLoading" message="Loading users..." />
      
      <div v-else class="management-container">
        <!-- Success Message -->
        <div v-if="successMessage" class="alert alert-success">
          {{ successMessage }}
        </div>
        
        <!-- Error Message -->
        <ErrorMessage v-if="generalError" :message="generalError" @close="clearGeneralError" />
        
        <!-- Action Bar -->
        <div class="action-bar">
          <h2>User List</h2>
          <div class="action-buttons">
            <button class="btn btn-primary" @click="openCreateDialog">
              <span>+ Create User</span>
            </button>
            <button class="btn btn-secondary" @click="returnToAdminMenu">
              Return to Admin Menu
            </button>
          </div>
        </div>
        
        <!-- Filter Bar -->
        <div class="filter-bar">
          <div class="form-group">
            <label for="userTypeFilter">Filter by User Type:</label>
            <select id="userTypeFilter" v-model="userTypeFilter" @change="loadUsers" class="form-control">
              <option value="">All Users</option>
              <option value="A">Admin (A)</option>
              <option value="U">User (U)</option>
            </select>
          </div>
        </div>

        <!-- User Table -->
        <div class="table-container">
          <table class="user-table">
            <thead>
              <tr>
                <th>User ID</th>
                <th>First Name</th>
                <th>Last Name</th>
                <th>User Type</th>
                <th>Actions</th>
              </tr>
            </thead>
            <tbody>
              <tr v-if="users.length === 0">
                <td colspan="5" class="no-data">No users found</td>
              </tr>
              <tr v-for="user in users" :key="user.userId">
                <td>{{ user.userId }}</td>
                <td>{{ user.firstName }}</td>
                <td>{{ user.lastName }}</td>
                <td>
                  <span class="badge" :class="user.userType === 'A' ? 'badge-admin' : 'badge-user'">
                    {{ user.userType === 'A' ? 'Admin' : 'User' }}
                  </span>
                </td>
                <td class="actions">
                  <button class="btn-icon btn-edit" @click="openUpdateDialog(user)" title="Edit User">
                    ✏️
                  </button>
                  <button class="btn-icon btn-delete" @click="confirmDeactivate(user)" title="Deactivate User">
                    🗑️
                  </button>
                </td>
              </tr>
            </tbody>
          </table>
        </div>

        <!-- Pagination -->
        <div v-if="totalPages > 1" class="pagination">
          <button 
            class="btn btn-sm" 
            @click="previousPage" 
            :disabled="currentPage === 0"
          >
            Previous
          </button>
          <span class="page-info">
            Page {{ currentPage + 1 }} of {{ totalPages }}
          </span>
          <button 
            class="btn btn-sm" 
            @click="nextPage" 
            :disabled="currentPage >= totalPages - 1"
          >
            Next
          </button>
        </div>
      </div>
    </div>
    
    <!-- Create User Dialog -->
    <div v-if="showCreateDialog" class="modal-overlay" @click.self="closeCreateDialog">
      <div class="modal-dialog">
        <div class="modal-header">
          <h3>Create New User</h3>
          <button class="btn-close" @click="closeCreateDialog">×</button>
        </div>
        <div class="modal-body">
          <ErrorMessage v-if="dialogError" :message="dialogError" @close="clearDialogError" />
          
          <div class="form-group">
            <label for="createUserId">User ID *</label>
            <input
              id="createUserId"
              v-model="newUser.userId"
              type="text"
              maxlength="8"
              class="form-control"
              :class="{ 'is-invalid': dialogFieldErrors.userId }"
              placeholder="8 characters"
            />
            <div v-if="dialogFieldErrors.userId" class="error-message">
              {{ dialogFieldErrors.userId }}
            </div>
          </div>

          <div class="form-group">
            <label for="createFirstName">First Name *</label>
            <input
              id="createFirstName"
              v-model="newUser.firstName"
              type="text"
              maxlength="20"
              class="form-control"
              :class="{ 'is-invalid': dialogFieldErrors.firstName }"
            />
            <div v-if="dialogFieldErrors.firstName" class="error-message">
              {{ dialogFieldErrors.firstName }}
            </div>
          </div>
          
          <div class="form-group">
            <label for="createLastName">Last Name *</label>
            <input
              id="createLastName"
              v-model="newUser.lastName"
              type="text"
              maxlength="20"
              class="form-control"
              :class="{ 'is-invalid': dialogFieldErrors.lastName }"
            />
            <div v-if="dialogFieldErrors.lastName" class="error-message">
              {{ dialogFieldErrors.lastName }}
            </div>
          </div>
          
          <div class="form-group">
            <label for="createPassword">Password *</label>
            <input
              id="createPassword"
              v-model="newUser.password"
              type="password"
              maxlength="8"
              class="form-control"
              :class="{ 'is-invalid': dialogFieldErrors.password }"
              placeholder="8 characters"
            />
            <div v-if="dialogFieldErrors.password" class="error-message">
              {{ dialogFieldErrors.password }}
            </div>
          </div>
          
          <div class="form-group">
            <label for="createUserType">User Type *</label>
            <select
              id="createUserType"
              v-model="newUser.userType"
              class="form-control"
              :class="{ 'is-invalid': dialogFieldErrors.userType }"
            >
              <option value="">Select Type</option>
              <option value="A">Admin (A)</option>
              <option value="U">User (U)</option>
            </select>
            <div v-if="dialogFieldErrors.userType" class="error-message">
              {{ dialogFieldErrors.userType }}
            </div>
          </div>
        </div>
        <div class="modal-footer">
          <button class="btn btn-primary" @click="createUser" :disabled="dialogSaving">
            <LoadingSpinner v-if="dialogSaving" size="small" />
            <span v-else>Create</span>
          </button>
          <button class="btn btn-secondary" @click="closeCreateDialog" :disabled="dialogSaving">
            Cancel
          </button>
        </div>
      </div>
    </div>

    <!-- Update User Dialog -->
    <div v-if="showUpdateDialog" class="modal-overlay" @click.self="closeUpdateDialog">
      <div class="modal-dialog">
        <div class="modal-header">
          <h3>Update User</h3>
          <button class="btn-close" @click="closeUpdateDialog">×</button>
        </div>
        <div class="modal-body">
          <ErrorMessage v-if="dialogError" :message="dialogError" @close="clearDialogError" />
          
          <div class="form-group">
            <label for="updateUserId">User ID</label>
            <input
              id="updateUserId"
              v-model="selectedUser.userId"
              type="text"
              class="form-control"
              disabled
              readonly
            />
          </div>
          
          <div class="form-group">
            <label for="updateFirstName">First Name *</label>
            <input
              id="updateFirstName"
              v-model="selectedUser.firstName"
              type="text"
              maxlength="20"
              class="form-control"
              :class="{ 'is-invalid': dialogFieldErrors.firstName }"
            />
            <div v-if="dialogFieldErrors.firstName" class="error-message">
              {{ dialogFieldErrors.firstName }}
            </div>
          </div>
          
          <div class="form-group">
            <label for="updateLastName">Last Name *</label>
            <input
              id="updateLastName"
              v-model="selectedUser.lastName"
              type="text"
              maxlength="20"
              class="form-control"
              :class="{ 'is-invalid': dialogFieldErrors.lastName }"
            />
            <div v-if="dialogFieldErrors.lastName" class="error-message">
              {{ dialogFieldErrors.lastName }}
            </div>
          </div>
          
          <div class="form-group">
            <label for="updatePassword">Password (leave blank to keep current)</label>
            <input
              id="updatePassword"
              v-model="selectedUser.password"
              type="password"
              maxlength="8"
              class="form-control"
              :class="{ 'is-invalid': dialogFieldErrors.password }"
              placeholder="8 characters"
            />
            <div v-if="dialogFieldErrors.password" class="error-message">
              {{ dialogFieldErrors.password }}
            </div>
          </div>
          
          <div class="form-group">
            <label for="updateUserType">User Type *</label>
            <select
              id="updateUserType"
              v-model="selectedUser.userType"
              class="form-control"
              :class="{ 'is-invalid': dialogFieldErrors.userType }"
            >
              <option value="A">Admin (A)</option>
              <option value="U">User (U)</option>
            </select>
            <div v-if="dialogFieldErrors.userType" class="error-message">
              {{ dialogFieldErrors.userType }}
            </div>
          </div>
        </div>
        <div class="modal-footer">
          <button class="btn btn-primary" @click="updateUser" :disabled="dialogSaving">
            <LoadingSpinner v-if="dialogSaving" size="small" />
            <span v-else>Update</span>
          </button>
          <button class="btn btn-secondary" @click="closeUpdateDialog" :disabled="dialogSaving">
            Cancel
          </button>
        </div>
      </div>
    </div>

    <!-- Deactivate Confirmation Dialog -->
    <div v-if="showDeactivateDialog" class="modal-overlay" @click.self="closeDeactivateDialog">
      <div class="modal-dialog modal-sm">
        <div class="modal-header">
          <h3>Confirm Deactivation</h3>
          <button class="btn-close" @click="closeDeactivateDialog">×</button>
        </div>
        <div class="modal-body">
          <p>Are you sure you want to deactivate user <strong>{{ userToDeactivate?.userId }}</strong>?</p>
          <p class="warning-text">This action cannot be undone.</p>
        </div>
        <div class="modal-footer">
          <button class="btn btn-danger" @click="deactivateUser" :disabled="dialogSaving">
            <LoadingSpinner v-if="dialogSaving" size="small" />
            <span v-else>Deactivate</span>
          </button>
          <button class="btn btn-secondary" @click="closeDeactivateDialog" :disabled="dialogSaving">
            Cancel
          </button>
        </div>
      </div>
    </div>
  </div>
</template>


<script>
import { ref, reactive, onMounted } from 'vue'
import { useRouter } from 'vue-router'
import userService from '../services/userService'
import authService from '../services/authService'
import LoadingSpinner from '../components/LoadingSpinner.vue'
import ErrorMessage from '../components/ErrorMessage.vue'

export default {
  name: 'UserManagement',
  components: {
    LoadingSpinner,
    ErrorMessage
  },
  setup() {
    const router = useRouter()
    
    // State
    const initialLoading = ref(true)
    const users = ref([])
    const currentPage = ref(0)
    const totalPages = ref(0)
    const pageSize = ref(20)
    const userTypeFilter = ref('')
    
    // Messages
    const successMessage = ref('')
    const generalError = ref('')
    
    // Dialog state
    const showCreateDialog = ref(false)
    const showUpdateDialog = ref(false)
    const showDeactivateDialog = ref(false)
    const dialogSaving = ref(false)
    const dialogError = ref('')
    const dialogFieldErrors = reactive({})
    
    // User data
    const newUser = reactive({
      userId: '',
      firstName: '',
      lastName: '',
      password: '',
      userType: ''
    })
    
    const selectedUser = ref(null)
    const userToDeactivate = ref(null)

    /**
     * Check if current user is admin
     * Requirements: 8.1
     */
    const checkAdminAuthorization = () => {
      const userInfo = authService.getUserInfo()
      if (!userInfo || userInfo.userType !== 'A') {
        generalError.value = 'No access - Admin Only option...'
        setTimeout(() => {
          router.push('/menu')
        }, 2000)
        return false
      }
      return true
    }


    /**
     * Load users with pagination
     * Requirements: 8.1
     */
    const loadUsers = async () => {
      if (!checkAdminAuthorization()) return
      
      initialLoading.value = true
      generalError.value = ''
      
      try {
        const response = await userService.listUsers(
          userTypeFilter.value || null,
          currentPage.value,
          pageSize.value
        )
        
        // Handle paginated response
        if (response.content) {
          users.value = response.content
          totalPages.value = response.totalPages || 1
        } else if (Array.isArray(response)) {
          // Handle non-paginated response
          users.value = response
          totalPages.value = 1
        } else {
          users.value = []
          totalPages.value = 0
        }
      } catch (error) {
        generalError.value = error.message || 'Failed to load users'
        users.value = []
      } finally {
        initialLoading.value = false
      }
    }

    /**
     * Navigate to previous page
     */
    const previousPage = () => {
      if (currentPage.value > 0) {
        currentPage.value--
        loadUsers()
      }
    }

    /**
     * Navigate to next page
     */
    const nextPage = () => {
      if (currentPage.value < totalPages.value - 1) {
        currentPage.value++
        loadUsers()
      }
    }

    /**
     * Open create user dialog
     * Requirements: 8.2
     */
    const openCreateDialog = () => {
      // Reset form
      newUser.userId = ''
      newUser.firstName = ''
      newUser.lastName = ''
      newUser.password = ''
      newUser.userType = ''
      
      // Clear errors
      Object.keys(dialogFieldErrors).forEach(key => delete dialogFieldErrors[key])
      dialogError.value = ''
      
      showCreateDialog.value = true
    }

    /**
     * Close create user dialog
     */
    const closeCreateDialog = () => {
      showCreateDialog.value = false
    }


    /**
     * Validate user form data
     */
    const validateUserForm = (userData, isUpdate = false) => {
      const errors = {}
      
      if (!isUpdate) {
        // User ID validation (required for create)
        if (!userData.userId || userData.userId.trim() === '') {
          errors.userId = 'User ID is required'
        } else if (userData.userId.length !== 8) {
          errors.userId = 'User ID must be exactly 8 characters'
        }
      }
      
      // First name validation
      if (!userData.firstName || userData.firstName.trim() === '') {
        errors.firstName = 'First Name is required'
      } else if (userData.firstName.length > 20) {
        errors.firstName = 'First Name must not exceed 20 characters'
      }
      
      // Last name validation
      if (!userData.lastName || userData.lastName.trim() === '') {
        errors.lastName = 'Last Name is required'
      } else if (userData.lastName.length > 20) {
        errors.lastName = 'Last Name must not exceed 20 characters'
      }
      
      // Password validation
      if (!isUpdate || (userData.password && userData.password.trim() !== '')) {
        if (!userData.password || userData.password.trim() === '') {
          errors.password = 'Password is required'
        } else if (userData.password.length !== 8) {
          errors.password = 'Password must be exactly 8 characters'
        }
      }
      
      // User type validation
      if (!userData.userType || !['A', 'U'].includes(userData.userType)) {
        errors.userType = 'User Type must be A (Admin) or U (User)'
      }
      
      return errors
    }

    /**
     * Create new user
     * Requirements: 8.2, 8.3
     */
    const createUser = async () => {
      // Clear previous errors
      Object.keys(dialogFieldErrors).forEach(key => delete dialogFieldErrors[key])
      dialogError.value = ''
      
      // Validate form
      const errors = validateUserForm(newUser, false)
      if (Object.keys(errors).length > 0) {
        Object.assign(dialogFieldErrors, errors)
        dialogError.value = 'Please correct the errors in the form'
        return
      }
      
      dialogSaving.value = true
      
      try {
        await userService.createUser({
          userId: newUser.userId.toUpperCase(), // Normalize to uppercase
          firstName: newUser.firstName,
          lastName: newUser.lastName,
          password: newUser.password,
          userType: newUser.userType
        })
        
        successMessage.value = `User ${newUser.userId.toUpperCase()} created successfully`
        closeCreateDialog()
        
        // Reload user list
        await loadUsers()
        
        // Clear success message after 5 seconds
        setTimeout(() => {
          successMessage.value = ''
        }, 5000)
      } catch (error) {
        if (error.fieldErrors) {
          error.fieldErrors.forEach(fieldError => {
            dialogFieldErrors[fieldError.field] = fieldError.message
          })
          dialogError.value = 'Please correct the errors in the form'
        } else {
          dialogError.value = error.message || 'Failed to create user'
        }
      } finally {
        dialogSaving.value = false
      }
    }


    /**
     * Open update user dialog
     * Requirements: 8.4
     */
    const openUpdateDialog = (user) => {
      selectedUser.value = {
        userId: user.userId,
        firstName: user.firstName,
        lastName: user.lastName,
        password: '', // Don't pre-fill password
        userType: user.userType
      }
      
      // Clear errors
      Object.keys(dialogFieldErrors).forEach(key => delete dialogFieldErrors[key])
      dialogError.value = ''
      
      showUpdateDialog.value = true
    }

    /**
     * Close update user dialog
     */
    const closeUpdateDialog = () => {
      showUpdateDialog.value = false
      selectedUser.value = null
    }

    /**
     * Update user
     * Requirements: 8.4
     */
    const updateUser = async () => {
      if (!selectedUser.value) return
      
      // Clear previous errors
      Object.keys(dialogFieldErrors).forEach(key => delete dialogFieldErrors[key])
      dialogError.value = ''
      
      // Validate form
      const errors = validateUserForm(selectedUser.value, true)
      if (Object.keys(errors).length > 0) {
        Object.assign(dialogFieldErrors, errors)
        dialogError.value = 'Please correct the errors in the form'
        return
      }
      
      dialogSaving.value = true
      
      try {
        const updates = {
          firstName: selectedUser.value.firstName,
          lastName: selectedUser.value.lastName,
          userType: selectedUser.value.userType
        }
        
        // Only include password if it was changed
        if (selectedUser.value.password && selectedUser.value.password.trim() !== '') {
          updates.password = selectedUser.value.password
        }
        
        await userService.updateUser(selectedUser.value.userId, updates)
        
        successMessage.value = `User ${selectedUser.value.userId} updated successfully`
        closeUpdateDialog()
        
        // Reload user list
        await loadUsers()
        
        // Clear success message after 5 seconds
        setTimeout(() => {
          successMessage.value = ''
        }, 5000)
      } catch (error) {
        if (error.fieldErrors) {
          error.fieldErrors.forEach(fieldError => {
            dialogFieldErrors[fieldError.field] = fieldError.message
          })
          dialogError.value = 'Please correct the errors in the form'
        } else {
          dialogError.value = error.message || 'Failed to update user'
        }
      } finally {
        dialogSaving.value = false
      }
    }


    /**
     * Open deactivate confirmation dialog
     * Requirements: 8.5
     */
    const confirmDeactivate = (user) => {
      userToDeactivate.value = user
      showDeactivateDialog.value = true
    }

    /**
     * Close deactivate confirmation dialog
     */
    const closeDeactivateDialog = () => {
      showDeactivateDialog.value = false
      userToDeactivate.value = null
    }

    /**
     * Deactivate user
     * Requirements: 8.5, 8.6
     */
    const deactivateUser = async () => {
      if (!userToDeactivate.value) return
      
      dialogSaving.value = true
      
      try {
        await userService.deactivateUser(userToDeactivate.value.userId)
        
        successMessage.value = `User ${userToDeactivate.value.userId} deactivated successfully`
        closeDeactivateDialog()
        
        // Reload user list
        await loadUsers()
        
        // Clear success message after 5 seconds
        setTimeout(() => {
          successMessage.value = ''
        }, 5000)
      } catch (error) {
        generalError.value = error.message || 'Failed to deactivate user'
        closeDeactivateDialog()
      } finally {
        dialogSaving.value = false
      }
    }

    /**
     * Return to admin menu
     */
    const returnToAdminMenu = () => {
      router.push('/admin-menu')
    }

    /**
     * Clear general error message
     */
    const clearGeneralError = () => {
      generalError.value = ''
    }

    /**
     * Clear dialog error message
     */
    const clearDialogError = () => {
      dialogError.value = ''
    }

    // Load users on mount
    onMounted(() => {
      if (checkAdminAuthorization()) {
        loadUsers()
      }
    })

    return {
      initialLoading,
      users,
      currentPage,
      totalPages,
      userTypeFilter,
      successMessage,
      generalError,
      showCreateDialog,
      showUpdateDialog,
      showDeactivateDialog,
      dialogSaving,
      dialogError,
      dialogFieldErrors,
      newUser,
      selectedUser,
      userToDeactivate,
      loadUsers,
      previousPage,
      nextPage,
      openCreateDialog,
      closeCreateDialog,
      createUser,
      openUpdateDialog,
      closeUpdateDialog,
      updateUser,
      confirmDeactivate,
      closeDeactivateDialog,
      deactivateUser,
      returnToAdminMenu,
      clearGeneralError,
      clearDialogError
    }
  }
}
</script>


<style scoped>
.user-management {
  min-height: 100vh;
  background-color: #f5f5f5;
}

.container {
  max-width: 1400px;
  margin: 0 auto;
  padding: 2rem;
}

.management-container {
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

.action-bar {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 1.5rem;
  padding-bottom: 1rem;
  border-bottom: 2px solid #007bff;
}

.action-bar h2 {
  margin: 0;
  color: #333;
  font-size: 1.5rem;
}

.action-buttons {
  display: flex;
  gap: 1rem;
}

.filter-bar {
  margin-bottom: 1.5rem;
  padding: 1rem;
  background-color: #f8f9fa;
  border-radius: 4px;
}

.filter-bar .form-group {
  display: flex;
  align-items: center;
  gap: 1rem;
  margin: 0;
}

.filter-bar label {
  margin: 0;
  font-weight: 600;
  white-space: nowrap;
}

.filter-bar .form-control {
  max-width: 200px;
}

.table-container {
  overflow-x: auto;
  margin-bottom: 1.5rem;
}

.user-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.95rem;
}

.user-table thead {
  background-color: #f8f9fa;
}

.user-table th {
  padding: 1rem;
  text-align: left;
  font-weight: 600;
  color: #333;
  border-bottom: 2px solid #dee2e6;
}

.user-table td {
  padding: 1rem;
  border-bottom: 1px solid #dee2e6;
}

.user-table tbody tr:hover {
  background-color: #f8f9fa;
}

.no-data {
  text-align: center;
  color: #6c757d;
  font-style: italic;
}

.badge {
  display: inline-block;
  padding: 0.25rem 0.75rem;
  border-radius: 12px;
  font-size: 0.875rem;
  font-weight: 600;
}

.badge-admin {
  background-color: #dc3545;
  color: white;
}

.badge-user {
  background-color: #007bff;
  color: white;
}

.actions {
  display: flex;
  gap: 0.5rem;
}

.btn-icon {
  background: none;
  border: none;
  font-size: 1.25rem;
  cursor: pointer;
  padding: 0.25rem 0.5rem;
  border-radius: 4px;
  transition: background-color 0.2s;
}

.btn-icon:hover {
  background-color: #f8f9fa;
}

.btn-edit:hover {
  background-color: #e7f3ff;
}

.btn-delete:hover {
  background-color: #ffe7e7;
}


.pagination {
  display: flex;
  justify-content: center;
  align-items: center;
  gap: 1rem;
  padding-top: 1rem;
  border-top: 1px solid #dee2e6;
}

.page-info {
  font-weight: 600;
  color: #333;
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
  gap: 0.5rem;
}

.btn-sm {
  padding: 0.5rem 1rem;
  font-size: 0.875rem;
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

.btn-danger {
  background-color: #dc3545;
  color: white;
}

.btn-danger:hover:not(:disabled) {
  background-color: #c82333;
}

.btn:disabled {
  opacity: 0.6;
  cursor: not-allowed;
}

/* Modal Styles */
.modal-overlay {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background-color: rgba(0, 0, 0, 0.5);
  display: flex;
  align-items: center;
  justify-content: center;
  z-index: 1000;
}

.modal-dialog {
  background: white;
  border-radius: 8px;
  box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  width: 90%;
  max-width: 500px;
  max-height: 90vh;
  overflow-y: auto;
}

.modal-dialog.modal-sm {
  max-width: 400px;
}

.modal-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1.5rem;
  border-bottom: 1px solid #dee2e6;
}

.modal-header h3 {
  margin: 0;
  color: #333;
  font-size: 1.25rem;
}

.btn-close {
  background: none;
  border: none;
  font-size: 2rem;
  line-height: 1;
  cursor: pointer;
  color: #6c757d;
  padding: 0;
  width: 2rem;
  height: 2rem;
  display: flex;
  align-items: center;
  justify-content: center;
}

.btn-close:hover {
  color: #000;
}

.modal-body {
  padding: 1.5rem;
}

.modal-footer {
  display: flex;
  justify-content: flex-end;
  gap: 1rem;
  padding: 1.5rem;
  border-top: 1px solid #dee2e6;
}

.form-group {
  margin-bottom: 1.5rem;
}

.form-group:last-child {
  margin-bottom: 0;
}

.form-group label {
  display: block;
  margin-bottom: 0.5rem;
  font-weight: 600;
  color: #333;
  font-size: 0.875rem;
}

.form-control {
  width: 100%;
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

.form-control:disabled {
  background-color: #e9ecef;
  cursor: not-allowed;
}

.error-message {
  color: #dc3545;
  font-size: 0.875rem;
  margin-top: 0.25rem;
}

.warning-text {
  color: #856404;
  background-color: #fff3cd;
  border: 1px solid #ffeaa7;
  padding: 0.75rem;
  border-radius: 4px;
  margin-top: 1rem;
}

@media (max-width: 768px) {
  .container {
    padding: 1rem;
  }
  
  .action-bar {
    flex-direction: column;
    align-items: flex-start;
    gap: 1rem;
  }
  
  .action-buttons {
    width: 100%;
    flex-direction: column;
  }
  
  .action-buttons .btn {
    width: 100%;
  }
  
  .filter-bar .form-group {
    flex-direction: column;
    align-items: flex-start;
  }
  
  .filter-bar .form-control {
    max-width: 100%;
  }
  
  .user-table {
    font-size: 0.875rem;
  }
  
  .user-table th,
  .user-table td {
    padding: 0.5rem;
  }
  
  .modal-dialog {
    width: 95%;
  }
}
</style>
