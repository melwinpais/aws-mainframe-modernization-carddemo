package com.carddemo.repository;

import com.carddemo.entity.User;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * JPA Repository for User entity
 * Provides data access methods for the users table
 */
@Repository
public interface UserRepository extends JpaRepository<User, String> {

    /**
     * Find user by user ID (case-insensitive)
     * @param userId the user ID to search for
     * @return Optional containing the user if found
     */
    Optional<User> findByUserIdIgnoreCase(String userId);

    /**
     * Find all users by user type
     * @param userType the user type ('A' for Admin, 'U' for User)
     * @return List of users with the specified type
     */
    List<User> findByUserType(String userType);

    /**
     * Find all users by user type with pagination
     * @param userType the user type ('A' for Admin, 'U' for User)
     * @param pageable pagination parameters
     * @return Page of users with the specified type
     */
    Page<User> findByUserType(String userType, Pageable pageable);

    /**
     * Check if a user exists by user ID (case-insensitive)
     * @param userId the user ID to check
     * @return true if user exists, false otherwise
     */
    boolean existsByUserIdIgnoreCase(String userId);
}
