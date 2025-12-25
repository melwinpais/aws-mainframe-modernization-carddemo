package com.carddemo.service;

import com.carddemo.dto.UserCreateDto;
import com.carddemo.dto.UserDto;
import com.carddemo.dto.UserUpdateDto;
import com.carddemo.entity.User;
import com.carddemo.exception.ResourceNotFoundException;
import com.carddemo.exception.ValidationException;
import com.carddemo.repository.UserRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Implementation of UserService
 * Handles user management operations with password hashing and validation
 */
@Service
@Transactional
public class UserServiceImpl implements UserService {

    private static final Logger logger = LoggerFactory.getLogger(UserServiceImpl.class);

    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final AuthService authService;

    public UserServiceImpl(UserRepository userRepository, 
                          PasswordEncoder passwordEncoder,
                          AuthService authService) {
        this.userRepository = userRepository;
        this.passwordEncoder = passwordEncoder;
        this.authService = authService;
    }

    @Override
    @Transactional(readOnly = true)
    public Page<UserDto> listUsers(String userType, Pageable pageable) {
        logger.info("Listing users with type filter: {}, page: {}, size: {}", 
                    userType, pageable.getPageNumber(), pageable.getPageSize());

        Page<User> users;
        if (userType != null && !userType.trim().isEmpty()) {
            // Validate user type
            if (!userType.matches("[AU]")) {
                throw new ValidationException("User type must be 'A' (Admin) or 'U' (User)");
            }
            // Use repository method to filter by user type
            users = userRepository.findByUserType(userType, pageable);
        } else {
            users = userRepository.findAll(pageable);
        }

        logger.info("Found {} users", users.getTotalElements());
        return users.map(this::convertToDto);
    }

    @Override
    @Transactional(readOnly = true)
    public UserDto getUser(String userId) {
        logger.info("Getting user: {}", userId);

        // Normalize user ID to uppercase
        String normalizedUserId = authService.normalizeUserId(userId);

        User user = userRepository.findByUserIdIgnoreCase(normalizedUserId)
                .orElseThrow(() -> {
                    logger.warn("User not found: {}", normalizedUserId);
                    return new ResourceNotFoundException("User not found: " + normalizedUserId);
                });

        logger.info("User found: {}", normalizedUserId);
        return convertToDto(user);
    }

    @Override
    public UserDto createUser(UserCreateDto userCreateDto) {
        logger.info("Creating new user: {}", userCreateDto.getUserId());

        // Normalize user ID to uppercase
        String normalizedUserId = authService.normalizeUserId(userCreateDto.getUserId());

        // Check if user already exists
        if (userRepository.existsByUserIdIgnoreCase(normalizedUserId)) {
            logger.warn("User already exists: {}", normalizedUserId);
            throw new ValidationException("User ID already exists: " + normalizedUserId);
        }

        // Create new user entity
        User user = new User();
        user.setUserId(normalizedUserId);
        user.setFirstName(userCreateDto.getFirstName());
        user.setLastName(userCreateDto.getLastName());
        user.setUserType(userCreateDto.getUserType());

        // Hash the password using bcrypt
        String hashedPassword = passwordEncoder.encode(userCreateDto.getPassword());
        user.setPassword(hashedPassword);

        // Save user
        User savedUser = userRepository.save(user);
        logger.info("User created successfully: {}", normalizedUserId);

        return convertToDto(savedUser);
    }

    @Override
    public UserDto updateUser(String userId, UserUpdateDto userUpdateDto) {
        logger.info("Updating user: {}", userId);

        // Normalize user ID to uppercase
        String normalizedUserId = authService.normalizeUserId(userId);

        // Find existing user
        User user = userRepository.findByUserIdIgnoreCase(normalizedUserId)
                .orElseThrow(() -> {
                    logger.warn("User not found for update: {}", normalizedUserId);
                    return new ResourceNotFoundException("User not found: " + normalizedUserId);
                });

        // Update fields if provided
        if (userUpdateDto.getFirstName() != null && !userUpdateDto.getFirstName().trim().isEmpty()) {
            user.setFirstName(userUpdateDto.getFirstName());
        }

        if (userUpdateDto.getLastName() != null && !userUpdateDto.getLastName().trim().isEmpty()) {
            user.setLastName(userUpdateDto.getLastName());
        }

        if (userUpdateDto.getUserType() != null && !userUpdateDto.getUserType().trim().isEmpty()) {
            user.setUserType(userUpdateDto.getUserType());
        }

        // Update password if provided (hash it)
        if (userUpdateDto.getPassword() != null && !userUpdateDto.getPassword().trim().isEmpty()) {
            String hashedPassword = passwordEncoder.encode(userUpdateDto.getPassword());
            user.setPassword(hashedPassword);
        }

        // Save updated user
        User updatedUser = userRepository.save(user);
        logger.info("User updated successfully: {}", normalizedUserId);

        return convertToDto(updatedUser);
    }

    @Override
    public void deactivateUser(String userId) {
        logger.info("Deactivating user: {}", userId);

        // Normalize user ID to uppercase
        String normalizedUserId = authService.normalizeUserId(userId);

        // Find existing user
        User user = userRepository.findByUserIdIgnoreCase(normalizedUserId)
                .orElseThrow(() -> {
                    logger.warn("User not found for deactivation: {}", normalizedUserId);
                    return new ResourceNotFoundException("User not found: " + normalizedUserId);
                });

        // Delete the user (hard delete as per requirements)
        userRepository.delete(user);
        logger.info("User deactivated successfully: {}", normalizedUserId);
    }

    /**
     * Convert User entity to UserDto
     * @param user the User entity
     * @return UserDto without password
     */
    private UserDto convertToDto(User user) {
        if (user == null) {
            return null;
        }
        return new UserDto(
                user.getUserId(),
                user.getFirstName(),
                user.getLastName(),
                user.getUserType(),
                user.getCreatedAt(),
                user.getUpdatedAt()
        );
    }
}
