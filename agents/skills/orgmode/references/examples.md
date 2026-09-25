# Org-Mode Examples

Real-world examples showing org-mode best practices and common patterns.

## Example 1: Meeting Notes

```org
#+TITLE: Team Meeting Notes
#+DATE: <2025-12-13 Fri>
#+FILETAGS: :meetings:team:

* Meeting: Sprint Planning <2025-12-13 Fri 10:00-11:30>
:PROPERTIES:
:CREATED: [2025-12-13 Fri 10:00]
:ATTENDEES: Alice Johnson, Bob Smith, Charlie Davis
:LOCATION: Conference Room B
:END:

** Agenda
1. Review last sprint
2. Plan current sprint
3. Discuss blockers
4. Action items

** Discussion Notes

*** Last Sprint Review
- Completed 23/25 story points
- Two items carried over to this sprint
- Team velocity trending upward

*** Current Sprint Planning
Committed items:
- [ ] User authentication feature (8 points)
- [ ] API endpoint refactoring (5 points)
- [ ] Bug fixes (3 points)

*** Blockers
- Waiting on API key from vendor (assigned to Alice)
- Database migration needs review (assigned to Bob)

** Action Items
*** TODO [#A] Request API key from vendor
DEADLINE: <2025-12-14 Sat>
:PROPERTIES:
:ASSIGNED_TO: Alice Johnson
:END:

*** TODO Review database migration
SCHEDULED: <2025-12-14 Sat>
:PROPERTIES:
:ASSIGNED_TO: Bob Smith
:EFFORT: 2:00
:END:

*** TODO Update sprint board
SCHEDULED: <2025-12-13 Fri 12:00>
:PROPERTIES:
:ASSIGNED_TO: Charlie Davis
:END:

** Next Meeting
SCHEDULED: <2025-12-20 Fri 10:00-11:30>
```

## Example 2: Project Plan

```org
#+TITLE: Website Redesign Project
#+AUTHOR: Project Team
#+DATE: 2025-01-10
#+FILETAGS: :projects:web:

* PROJECT Website Redesign
:PROPERTIES:
:ID: f8e3c2a1-9b4d-5e6f-7a8b-9c0d1e2f3a4b
:CUSTOM_ID: website-redesign
:CATEGORY: projects
:CLIENT: Acme Corporation
:BUDGET: 50000
:START_DATE: <2025-01-15 Wed>
:END:
SCHEDULED: <2025-01-15 Wed>
DEADLINE: <2025-03-31 Mon -7d>

** Overview
Complete redesign of company website with modern UI/UX, responsive design,
and improved performance.

** Objectives
- [ ] Modernize visual design
- [ ] Improve mobile responsiveness
- [ ] Increase page load speed by 50%
- [ ] Implement new CMS

** Phases

*** DONE [#A] Phase 1: Discovery & Planning
CLOSED: [2025-01-14 Tue] SCHEDULED: <2025-01-01 Wed> DEADLINE: <2025-01-14 Tue>
:PROPERTIES:
:EFFORT: 40:00
:PROGRESS: 100
:END:

**** DONE Requirements gathering
CLOSED: [2025-01-07 Tue]
- Stakeholder interviews
- User research
- Competitive analysis

**** DONE Design brief
CLOSED: [2025-01-14 Tue]
Created comprehensive design brief with goals, audience, and brand guidelines.

*** IN-PROGRESS [#A] Phase 2: Design
SCHEDULED: <2025-01-15 Wed> DEADLINE: <2025-02-15 Sat>
:PROPERTIES:
:EFFORT: 120:00
:PROGRESS: 60
:ASSIGNED_TO: Design Team
:END:

**** DONE Wireframes
CLOSED: [2025-01-28 Tue]
:PROPERTIES:
:EFFORT: 40:00
:END:

[[file:assets/wireframes-v2.pdf][Wireframes v2]]

**** IN-PROGRESS Visual design
SCHEDULED: <2025-01-29 Wed> DEADLINE: <2025-02-15 Sat>
:PROPERTIES:
:EFFORT: 60:00
:PROGRESS: 75
:END:

***** DONE Homepage design
***** DONE Product page design
***** IN-PROGRESS About page design
***** TODO Contact page design

**** TODO Design system documentation
DEADLINE: <2025-02-15 Sat>

*** TODO [#A] Phase 3: Development
SCHEDULED: <2025-02-16 Sun> DEADLINE: <2025-03-20 Thu>
:PROPERTIES:
:EFFORT: 200:00
:ASSIGNED_TO: Development Team
:END:

**** TODO Frontend development
**** TODO Backend development
**** TODO CMS integration
**** TODO Testing

*** TODO [#B] Phase 4: Launch
SCHEDULED: <2025-03-21 Fri> DEADLINE: <2025-03-31 Mon>
:PROPERTIES:
:EFFORT: 40:00
:END:

**** TODO Staging deployment
**** TODO Client review
**** TODO Production deployment
**** TODO Post-launch monitoring

** Resources
*** Team
- Alice Johnson - Project Manager
- Bob Smith - Lead Designer
- Charlie Davis - Lead Developer
- Diana Wilson - Content Strategist

*** Documents
- [[file:docs/requirements.org][Requirements Document]]
- [[file:docs/design-brief.org][Design Brief]]
- [[file:docs/technical-spec.org][Technical Specification]]

*** Assets
- [[file:assets/brand-guidelines.pdf][Brand Guidelines]]
- [[file:assets/wireframes-v2.pdf][Wireframes]]
- [[file:assets/mockups/][Design Mockups Directory]]

** Meeting Schedule
*** TODO Weekly status meeting
SCHEDULED: <2025-12-16 Mon 14:00 +1w>
:PROPERTIES:
:EFFORT: 1:00
:LOCATION: Conference Room A
:END:

** Notes & Decisions
*** Design Decision: Color Scheme
[2025-01-20 Mon]

Decided on primary blue (#0066CC) with accent orange (#FF6600).
Rationale: Better accessibility, aligns with brand refresh.

*** Technical Decision: CMS Platform
[2025-01-25 Sat]

Selected WordPress over custom solution due to client familiarity
and extensive plugin ecosystem.
```

## Example 3: Personal TODO List

```org
#+TITLE: Personal Tasks
#+FILETAGS: :personal:

* TODO [#A] File tax return
DEADLINE: <2025-04-15 Tue -14d>
:PROPERTIES:
:EFFORT: 3:00
:CONTEXT: home
:END:

** Subtasks
- [X] Gather W-2 forms
- [X] Collect receipts for deductions
- [ ] Complete tax software forms
- [ ] Review before submission
- [ ] Submit electronically

* TODO [#B] Schedule dentist appointment
SCHEDULED: <2025-12-14 Sat>
:PROPERTIES:
:PHONE: 555-0123
:CONTEXT: phone
:END:

* TODO [#C] Organize home office
:PROPERTIES:
:EFFORT: 4:00
:CONTEXT: home
:END:

Tasks:
- [ ] Sort papers
- [ ] Organize cables
- [ ] Clean desk
- [ ] Update filing system

* SOMEDAY Learn Python
:PROPERTIES:
:CREATED: [2025-11-15 Fri]
:RESOURCES: [[https://python.org][Python.org]], [[file:~/books/python-intro.pdf][Python Book]]
:END:

Potential learning resources and timeline to be determined.

* Recurring Tasks
** TODO Weekly review
SCHEDULED: <2025-12-16 Mon 09:00 +1w>
:PROPERTIES:
:EFFORT: 1:00
:REPEAT_TO_STATE: TODO
:END:

Review past week and plan next week.

** TODO Exercise
SCHEDULED: <2025-12-14 Sat 07:00 .+2d>
:PROPERTIES:
:EFFORT: 1:00
:END:

** TODO Pay rent
DEADLINE: <2025-12-31 Wed +1m>
:PROPERTIES:
:AMOUNT: 2000
:PAYEE: Landlord Name
:END:
```

## Example 4: Research Notes

```org
#+TITLE: Research: Machine Learning Applications
#+AUTHOR: Researcher Name
#+DATE: 2025-12-01
#+FILETAGS: :research:ml:

* Overview
Research on practical machine learning applications in healthcare.

* Literature Review

** Paper: Deep Learning for Medical Diagnosis
:PROPERTIES:
:CUSTOM_ID: zhang2024-dl
:AUTHORS: Zhang et al.
:YEAR: 2024
:DOI: 10.1234/ml.2024.5678
:STATUS: read
:RATING: 5
:END:

*** Summary
Comprehensive study on using deep learning for automated medical diagnosis.
Key findings include 95% accuracy in detecting certain conditions.

*** Key Points
- CNN architecture outperforms traditional methods
- Transfer learning reduces training time
- Validation on diverse dataset

*** Relevance
Directly applicable to current project on diagnostic tools.
See implementation notes in [[#implementation]].

*** Notes
- Important consideration on page 15 about data bias
- Figure 3 shows architecture diagram (worth replicating)
- Code available at [[https://github.com/example/ml-diagnosis][GitHub repo]]

** Paper: Ethics in AI Healthcare
:PROPERTIES:
:CUSTOM_ID: smith2024-ethics
:AUTHORS: Smith & Johnson
:YEAR: 2024
:STATUS: to-read
:PRIORITY: high
:END:

* Implementation Ideas
:PROPERTIES:
:CUSTOM_ID: implementation
:END:

** Prototype Architecture
Based on findings from [[#zhang2024-dl][Zhang et al.]], propose:

1. Data preprocessing pipeline
2. CNN model with transfer learning
3. Validation framework
4. Ethical review process (see [[#smith2024-ethics][Smith & Johnson]])

** Next Steps
*** TODO Review code repository
SCHEDULED: <2025-12-15 Mon>
:PROPERTIES:
:EFFORT: 2:00
:END:

Review implementation at [[https://github.com/example/ml-diagnosis][GitHub]].

*** TODO Draft technical proposal
DEADLINE: <2025-12-20 Fri>

*** TODO Consult with ethics board
SCHEDULED: <2025-12-22 Sun>

* References
1. Zhang, L., et al. (2024). "Deep Learning for Medical Diagnosis"
   [[doi:10.1234/ml.2024.5678]]

2. Smith, J., & Johnson, K. (2024). "Ethics in AI Healthcare"
   [[file:~/papers/smith-johnson-2024.pdf][Local PDF]]
```

## Example 5: Course Notes

```org
#+TITLE: CS 101: Introduction to Programming
#+AUTHOR: Student Name
#+FILETAGS: :education:cs101:

* Course Information
:PROPERTIES:
:INSTRUCTOR: Prof. Jane Smith
:EMAIL: jsmith@university.edu
:OFFICE_HOURS: Tue/Thu 2:00-3:30 PM
:LOCATION: Room 305
:END:

* Syllabus Overview
** Grading
| Component   | Percentage |
|-------------+------------|
| Homework    |        30% |
| Midterm     |        30% |
| Final       |        30% |
| Projects    |        10% |
|-------------+------------|
| Total       |       100% |

** Important Dates
- Midterm: <2025-02-15 Sat 14:00-16:00>
- Final: <2025-04-20 Sun 09:00-12:00>
- Project Due: <2025-04-10 Thu>

* Lecture Notes

** Week 1: Introduction <2025-01-10 Fri>
:PROPERTIES:
:TOPICS: Course overview, Setup, Hello World
:READING: Chapter 1-2
:END:

*** What is Programming?
Programming is giving instructions to a computer to perform tasks.

*** First Program
#+BEGIN_SRC python
print("Hello, World!")
#+END_SRC

*** Key Concepts
- Variables :: Storage for data values
- Functions :: Reusable code blocks
- Syntax :: Rules of the language

*** Homework
**** TODO [#A] Complete Chapter 1 exercises
DEADLINE: <2025-01-17 Fri>
:PROPERTIES:
:ESTIMATED_TIME: 2:00
:END:

** Week 2: Variables and Data Types <2025-01-17 Fri>
:PROPERTIES:
:TOPICS: Variables, Integers, Strings, Booleans
:READING: Chapter 3
:END:

*** Data Types
- int: Whole numbers
- float: Decimal numbers
- str: Text
- bool: True/False

*** Examples
#+BEGIN_SRC python
age = 25              # int
price = 19.99         # float
name = "Alice"        # str
is_student = True     # bool
#+END_SRC

*** Important Note
*Remember*: Python is dynamically typed - variables don't need explicit type declaration.

* Assignments

** TODO Homework 1: Variables
DEADLINE: <2025-01-20 Mon>
:PROPERTIES:
:POINTS: 10
:STATUS: in-progress
:END:

*** Requirements
- [ ] Problem 1: Variable declarations
- [X] Problem 2: Type conversions
- [ ] Problem 3: String operations

** TODO Project: Simple Calculator
DEADLINE: <2025-04-10 Thu>
:PROPERTIES:
:POINTS: 100
:STATUS: not-started
:END:

Build a command-line calculator supporting basic operations.

* Resources
** Textbook
[[file:~/textbooks/intro-to-programming.pdf][Introduction to Programming (PDF)]]

** Online Resources
- [[https://python.org/docs][Python Documentation]]
- [[https://stackoverflow.com][Stack Overflow]]
- [[file:code-examples/][Course Code Examples]]

** Study Group
Meeting weekly on <2025-01-18 Sat 15:00 +1w>
```

## Example 6: Recipe Collection

```org
#+TITLE: Recipe Collection
#+FILETAGS: :recipes:cooking:

* Breakfast

** Overnight Oats
:PROPERTIES:
:PREP_TIME: 5 min
:SERVINGS: 1
:DIFFICULTY: Easy
:RATING: 5
:END:

*** Ingredients
- 1/2 cup rolled oats
- 1/2 cup milk (dairy or non-dairy)
- 1/4 cup yogurt
- 1 tbsp chia seeds
- 1 tbsp honey
- Fresh berries for topping

*** Instructions
1. Combine oats, milk, yogurt, chia seeds, and honey in jar
2. Stir well to combine
3. Refrigerate overnight (or minimum 4 hours)
4. Top with fresh berries before serving

*** Notes
- Can prepare multiple servings on Sunday for the week
- Variations: Add peanut butter, banana, or cinnamon
- Stores well for up to 5 days

** Scrambled Eggs
:PROPERTIES:
:PREP_TIME: 2 min
:COOK_TIME: 5 min
:SERVINGS: 2
:DIFFICULTY: Easy
:END:

*** Ingredients
- 4 large eggs
- 2 tbsp milk
- Salt and pepper to taste
- 1 tbsp butter

*** Instructions
1. Beat eggs with milk, salt, and pepper
2. Melt butter in pan over medium heat
3. Pour in egg mixture
4. Gently stir until eggs are set but still soft
5. Remove from heat immediately

* Dinner

** Pasta Primavera
:PROPERTIES:
:PREP_TIME: 15 min
:COOK_TIME: 20 min
:SERVINGS: 4
:DIFFICULTY: Medium
:RATING: 4
:SOURCE: [[https://example.com/recipe][Recipe Source]]
:END:

*** Ingredients

**** Pasta
- 1 lb pasta (penne or fusilli)
- Salt for pasta water

**** Vegetables
- 2 cups broccoli florets
- 1 red bell pepper, sliced
- 1 yellow squash, sliced
- 1 cup cherry tomatoes, halved
- 3 cloves garlic, minced

**** Sauce
- 1/4 cup olive oil
- 1/2 cup pasta water
- 1/2 cup Parmesan cheese
- Fresh basil
- Salt and pepper

*** Instructions
1. Cook pasta according to package directions
2. Reserve 1 cup pasta water before draining
3. Sauté vegetables in olive oil until tender-crisp
4. Add garlic and cook 1 minute
5. Toss pasta with vegetables
6. Add pasta water and Parmesan
7. Season with salt, pepper, and fresh basil

*** Notes
- Great for using up vegetables in the fridge
- Add grilled chicken for protein
- Leftovers reheat well

* Meal Planning

** Week of <2025-12-16 Mon>

| Day | Breakfast      | Lunch          | Dinner           |
|-----+----------------+----------------+------------------|
| Mon | Overnight Oats | Leftovers      | Pasta Primavera  |
| Tue | Scrambled Eggs | Sandwich       | Stir Fry         |
| Wed | Overnight Oats | Salad          | Grilled Chicken  |
| Thu | Yogurt Parfait | Soup           | Pasta Primavera  |
| Fri | Scrambled Eggs | Leftovers      | Pizza Night      |
| Sat | Pancakes       | Brunch Out     | BBQ              |
| Sun | Overnight Oats | Meal Prep      | Roast Dinner     |

** TODO Grocery shopping
SCHEDULED: <2025-12-15 Sun>

*** Shopping List
**** Produce
- [ ] Broccoli
- [ ] Bell peppers
- [ ] Tomatoes
- [ ] Berries

**** Dairy
- [ ] Milk
- [ ] Yogurt
- [ ] Parmesan cheese

**** Pantry
- [ ] Oats
- [ ] Pasta
- [ ] Olive oil
```

These examples demonstrate proper org-mode structure, formatting, and real-world usage patterns.
