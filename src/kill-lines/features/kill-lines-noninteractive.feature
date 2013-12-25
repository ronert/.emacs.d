Feature: Use kill-to-line
  In order to kill multiple lines easily
  As an Emacs user
  I want to use kill-to-line
  
  Background:
    Given I am in buffer "Kill Lines Test"
    And I turn off linum-mode
    And I turn off hl-line-mode
    And I clear the buffer
    And I insert:
    """
    Line 1
    Line 2
    Line 3
    Line 4
    Line 5
    Line 6
    """
    And I go to line "1"

  Scenario: Kill Some Lines Forward
    When I go to line "3"
    And I kill until line 5
    Then I should see:
    """
    Line 1
    Line 2
    Line 6
    """

  Scenario: Kill Some Lines Backward
    When I go to line "5"
    And I kill until line 3
    Then I should see:
    """
    Line 1
    Line 2
    Line 6
    """

  Scenario: Target out of range: too small
    When I kill until line 0
    Then I should see message "Line number 0 out of range: 1, 6"

  Scenario: Target out of range: too large
    When I kill until line 7
    Then I should see message "Line number 7 out of range: 1, 6"
