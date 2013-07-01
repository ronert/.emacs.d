Feature: Use kill-lines Interactively
  In order to kill multiple lines easily
  As an Emacs user
  I want to use kill-lines
  
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

  Scenario: Kill lines interactively: no linum or hl-line
    When I start an action chain
    And I press "M-x"
    And I type "kill-lines"
    And I press "<return>"
    And I type "2"
    And I press "<return>"
    And I execute the action chain
    Then I should see:
    """
    Line 3
    Line 4
    Line 5
    Line 6
    """
    And linum should be "off"
    And hl-line should be "off"

  Scenario: Kill lines interactively: linum, no hl-line
    Then the cursor should be at point "1"
    When I start an action chain
    And I press "M-x"
    And I type "linum-mode"
    And I press "<return>"
    And I press "M-x"
    And I type "kill-lines"
    And I press "<return>"
    And I type "2"
    And I press "<return>"
    And I execute the action chain
    Then I should see:
    """
    Line 3
    Line 4
    Line 5
    Line 6
    """
    And linum should be "on"
    And hl-line should be "off"

  Scenario: Kill lines interactively: hl-line, no linum
    When I start an action chain
    And I press "M-x"
    And I type "hl-line-mode"
    And I press "<return>"
    And I press "M-x"
    And I type "kill-lines"
    And I press "<return>"
    And I type "2"
    And I press "<return>"
    And I execute the action chain
    Then I should see:
    """
    Line 3
    Line 4
    Line 5
    Line 6
    """
    And linum should be "off"
    And hl-line should be "on"

  Scenario: Kill lines interactively: linum and hl-line
    When I start an action chain
    And I press "M-x"
    And I type "linum-mode"
    And I press "<return>"
    And I press "M-x"
    And I type "hl-line-mode"
    And I press "<return>"
    And I press "M-x"
    And I type "kill-lines"
    And I press "<return>"
    And I type "2"
    And I press "<return>"
    And I execute the action chain
    Then I should see:
    """
    Line 3
    Line 4
    Line 5
    Line 6
    """
    And linum should be "on"
    And hl-line should be "on"

